public struct GLRParser<
  Terminal: SymbolProto, NonTerminal: SymbolProto, G: Grammar<Terminal, NonTerminal>
>: Parser {

  public enum ParseError: Error {
    case unexpectedTerminal(TerminalOrEnd)
  }

  public typealias G = G
  public typealias Terminal = Terminal
  public typealias NonTerminal = NonTerminal

  public typealias TerminalOrEnd = G.TerminalOrEnd
  public typealias Rule = G.Rule

  public typealias Item = LR1Parser<Terminal, NonTerminal, G>.Item
  public typealias RulePointer = LR1Parser<Terminal, NonTerminal, G>.RulePointer

  /// Maps items to valid lookahead terminals.
  private typealias ItemSet = OrderedDict<Item, BitSet>
  private typealias ItemSetID = Int

  private struct Transitions {
    var shift: [Symbol: OrderedSet<Int>] = [:]
    var reduce: [TerminalOrEnd: OrderedSet<RulePointer>] = [:]

    func expectedTerminals() -> OrderedSet<TerminalOrEnd> {
      OrderedSet(
        shift.keys.compactMap { symbol in
          if case .terminal(let t) = symbol {
            .terminal(t)
          } else {
            nil
          }
        }
      ).union(reduce.keys)
    }
  }

  private struct GSS {
    struct Node: Hashable {
      let position: Int
      let state: ItemSetID
    }

    var nodes = OrderedSet<Node>()
    var parentEdges = [Node: OrderedDict<Node, Match>]()
    var posToNode = [Int: OrderedSet<Node>]()

    @discardableResult
    mutating func insert(node: Node) -> Bool {
      let res = nodes.insert(node)
      posToNode[node.position, default: []].insert(node)
      return res
    }

    @discardableResult
    mutating func insertEdge(parent: Node, child: Node, match: Match) -> Bool {
      if parentEdges[child, default: [:]][parent] == nil {
        parentEdges[child, default: [:]][parent] = match
        return true
      } else {
        return false
      }
    }

    func climb(node: Node, hops: Int) -> [(Node, [Match])] {
      var results: [(Node, [Match])] = [(node, [])]
      for _ in 0..<hops {
        let oldResults = results
        results = []
        for (oldParent, oldMatch) in oldResults {
          for (newParent, newToken) in parentEdges[oldParent, default: [:]] {
            results.append((newParent, [newToken] + oldMatch))
          }
        }
      }
      return results
    }
  }

  private let grammar: G
  private let bitSetConverter: BitSetConverter<TerminalOrEnd>
  private let firstTerminals: [NonTerminal: BitSet]
  private var ruleMap = [NonTerminal: [RulePointer]]()
  private var itemSets = [ItemSet: ItemSetID]()
  private var transitionMap = [ItemSetID: Transitions]()
  private var firstFoundMatch: Match? = nil
  private var gss: GSS = GSS()
  private var parsedOffset: Int = 0

  public init(grammar: G) throws {
    let bsc = grammar.terminalBitSetConverter()
    self.grammar = grammar
    self.bitSetConverter = bsc
    self.firstTerminals = grammar.firstTerminals().mapValues(bsc.toBits)

    let rules = OrderedSet(grammar.rules).map { RulePointer(rule: $0) }

    for rule in rules {
      ruleMap[rule.lhs, default: []].append(rule)
    }

    let startItems = ruleMap[grammar.start, default: []].map { Item(rule: $0, offset: 0) }
    let seedItemSet = ItemSet(
      uniqueKeysWithValues: startItems.map { x in (x, bitSetConverter.toBits([.end])) }
    )
    let startItemSet = closure(seedItemSet)
    itemSets[startItemSet] = 0

    // Expand item sets for each symbol after a dot.
    var itemSetsToExpand = [startItemSet]
    while let itemSet = itemSetsToExpand.popLast() {
      let map = try expandItemSet(itemSet) { newItemSet in
        if let id = itemSets[newItemSet] {
          return id
        } else {
          let id = itemSets.count
          itemSets[newItemSet] = id
          itemSetsToExpand.append(newItemSet)
          return id
        }
      }
      transitionMap[itemSets[itemSet]!] = map
      assert(
        !map.expectedTerminals().isEmpty,
        "item set \(itemSet) has no terminal transitions, only \(map)"
      )
    }

    gss.insert(node: .init(position: 0, state: 0))
  }

  public mutating func put(terminal: Terminal) throws {
    try put(.terminal(terminal))
    parsedOffset += 1
    assert(firstFoundMatch == nil)
  }

  public mutating func end() throws -> Match {
    try put(.end)
    return firstFoundMatch!
  }

  private mutating func put(_ t: TerminalOrEnd) throws {
    precondition(firstFoundMatch == nil, "already found match")
    var pending = gss.posToNode[parsedOffset, default: []]
    while !pending.isEmpty {
      let curPending = pending
      pending = []
      for node in curPending {
        let map = transitionMap[node.state]!
        if case .terminal(let terminal) = t {
          for newState in map.shift[.terminal(terminal), default: []] {
            let newNode = GSS.Node(position: parsedOffset + 1, state: newState)
            gss.insert(node: newNode)
            gss.insertEdge(parent: node, child: newNode, match: .terminal(terminal))
          }
        }
        for reduce in map.reduce[t, default: []] {
          let popCount = reduce.rhs.count
          for (startNode, matches) in gss.climb(node: node, hops: popCount) {
            let newMatch = Match.nonTerminal(lhs: reduce.lhs, rhs: matches)
            let nextMap = transitionMap[startNode.state]!
            if gss.parentEdges[startNode, default: [:]].isEmpty && reduce.lhs == grammar.start
              && t == .end
            {
              firstFoundMatch = newMatch
              return
            }
            for nextState in nextMap.shift[.nonTerminal(reduce.lhs), default: []] {
              let newNode = GSS.Node(position: parsedOffset, state: nextState)
              gss.insert(node: newNode)
              if gss.insertEdge(parent: startNode, child: newNode, match: newMatch) {
                pending.insert(newNode)
              }
            }
          }
        }
      }
    }
    if gss.posToNode[parsedOffset + 1, default: []].isEmpty {
      throw ParseError.unexpectedTerminal(t)
    }
  }

  private func closure(_ iset: ItemSet) -> ItemSet {
    LR1Parser.closure(
      iset,
      ruleMap: ruleMap,
      firstTerminals: firstTerminals,
      bitSetConverter: bitSetConverter
    )
  }

  private func expandItemSet(_ iset: ItemSet, _ fn: (ItemSet) -> ItemSetID) throws -> Transitions {
    var transitions = Transitions(shift: [:], reduce: [:])

    func itemSetFor(next: Symbol) -> ItemSet {
      .init(
        uniqueKeysWithValues: iset.compactMap { (item, validTerminals) in
          if item.next != next {
            return nil
          }
          return (Item(rule: item.rule, offset: item.offset + 1), validTerminals)
        }
      )
    }

    for nextSymbol in OrderedSet(iset.keys.compactMap { $0.next }) {
      let nextID = fn(closure(itemSetFor(next: nextSymbol)))
      transitions.shift[nextSymbol, default: []].insert(nextID)
    }

    for (item, terminals) in iset {
      if item.next == nil {
        for t in bitSetConverter.fromBits(terminals) {
          transitions.reduce[t, default: []].insert(item.rule)
        }
      }
    }

    return transitions
  }
}

extension GLRParser: Sendable where GLRParser.G: Sendable {}
