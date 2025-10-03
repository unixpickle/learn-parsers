public enum LR1Error: Error {
  case notImplemented
}

public struct LR1Parser<
  Terminal: SymbolProto, NonTerminal: SymbolProto, G: Grammar<Terminal, NonTerminal>
>: Parser {

  public enum GrammarError: Error {
    case reduceReduce(TerminalOrEnd, Rule, Rule)
    case shiftReduce(Terminal, Item, [Item])
  }

  public enum ParseError: Error {
    case unexpectedTerminal(TerminalOrEnd, [TerminalOrEnd])
  }

  public typealias Terminal = Terminal
  public typealias NonTerminal = NonTerminal

  public typealias Rule = G.Rule

  public struct Item: Hashable, Sendable, CustomStringConvertible {
    let rule: G.Rule
    let offset: Int

    public var description: String {
      let pieces = rule.rhs.map { $0.description }
      let piecesWithDot = pieces[..<offset] + ["."] + pieces[offset...]
      return "Rule(\(rule.lhs) -> \(piecesWithDot.joined(separator: " ")))"
    }

    var next: Symbol? {
      offset < rule.rhs.count ? rule.rhs[offset] : nil
    }

    public func nextTerminals(
      lookaheadTerminals: Set<TerminalOrEnd>,
      firstTerminals: [NonTerminal: Set<Terminal>]
    ) -> Set<TerminalOrEnd> {
      if offset + 1 == rule.rhs.count {
        return lookaheadTerminals
      }
      switch rule.rhs[offset + 1] {
      case .terminal(let x):
        return [.terminal(x)]
      case .nonTerminal(let x):
        return Set(firstTerminals[x, default: []].map { .terminal($0) })
      }
    }
  }

  public enum TerminalOrEnd: Hashable, Sendable, CustomStringConvertible {
    case terminal(Terminal)
    case end

    public var description: String {
      switch self {
      case .terminal(let t): "terminal(\(t))"
      case .end: "end"
      }
    }
  }

  /// Maps items to valid lookahead terminals.
  private typealias ItemSet = [Item: Set<TerminalOrEnd>]
  private typealias ItemSetID = Int

  private struct Transitions {
    public var shift: [Symbol: Int] = [:]
    public var reduce: [TerminalOrEnd: Rule] = [:]

    func expectedTerminals() -> [TerminalOrEnd] {
      shift.keys.compactMap { symbol in
        if case .terminal(let t) = symbol {
          .terminal(t)
        } else {
          nil
        }
      } + reduce.keys
    }
  }

  private let grammar: G
  private let firstTerminals: [NonTerminal: Set<Terminal>]
  private var ruleMap = [NonTerminal: [Rule]]()
  private var itemSets = [ItemSet: ItemSetID]()
  private var transitionMap = [ItemSetID: Transitions]()

  private var stateStack: [ItemSetID] = []
  private var stateMatches: [Match] = []

  public init(grammar: G) throws {
    self.grammar = grammar
    self.firstTerminals = grammar.firstTerminals()

    for rule in grammar.rules {
      ruleMap[rule.lhs, default: []].append(rule)
    }

    let startItems = ruleMap[grammar.start, default: []].map { Item(rule: $0, offset: 0) }
    let seedItemSet: ItemSet = Dictionary(
      uniqueKeysWithValues: startItems.map { x in (x, [.end]) }
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

    stateStack.append(0)
  }

  public mutating func put(terminal: Terminal) throws {
    try put(.terminal(terminal))
  }

  public mutating func end() throws -> Match {
    try put(.end)
    assert(stateMatches.count == 1)
    return stateMatches.first!
  }

  private mutating func put(_ t: TerminalOrEnd) throws {
    while true {
      let state = stateStack.last!
      let map = transitionMap[state]!

      if case .terminal(let terminal) = t, let shift = map.shift[.terminal(terminal)] {
        stateStack.append(shift)
        stateMatches.append(.terminal(terminal))
        return
      } else if let reduce = map.reduce[t] {
        let popCount = reduce.rhs.count
        assert(stateStack.count > popCount)
        stateStack.removeLast(popCount)
        let matches = Array(stateMatches[(stateMatches.count - popCount)...])
        stateMatches.removeLast(popCount)
        stateMatches.append(.nonTerminal(lhs: reduce.lhs, rhs: matches))

        let newState = stateStack.last!
        let newMap = transitionMap[newState]!
        if newState == 0 && stateStack.count == 1 && reduce.lhs == grammar.start {
          stateStack.removeLast()
          return
        }
        guard let shift = newMap.shift[.nonTerminal(reduce.lhs)] else {
          fatalError("matched rule \(reduce) but cannot shift \(reduce.lhs) after popping stack")
        }
        stateStack.append(shift)
      } else {
        throw ParseError.unexpectedTerminal(
          t,
          map.expectedTerminals()
        )
      }
    }
  }

  private func closure(_ iset: ItemSet) -> ItemSet {
    var result = iset
    var converged = false
    while !converged {
      converged = true
      for (sourceItem, lookaheadTerminals) in result {
        if case .nonTerminal(let x) = sourceItem.next {
          let nextTerminals = sourceItem.nextTerminals(
            lookaheadTerminals: lookaheadTerminals,
            firstTerminals: firstTerminals
          )
          for expandRule in ruleMap[x, default: []] {
            let addedItem = Item(rule: expandRule, offset: 0)
            let oldSet = result[addedItem]
            let newSet = (oldSet ?? []).union(nextTerminals)
            if newSet.count > (oldSet?.count ?? 0) {
              result[addedItem] = newSet
              converged = false
            }
          }
        }
      }
    }
    return result
  }

  private func expandItemSet(_ iset: ItemSet, _ fn: (ItemSet) -> ItemSetID) throws -> Transitions {
    var transitions = Transitions(shift: [:], reduce: [:])

    func itemSetFor(next: Symbol) -> ItemSet {
      Dictionary(
        uniqueKeysWithValues: iset.compactMap { (item, validTerminals) in
          if item.next != next {
            return nil
          }
          return (Item(rule: item.rule, offset: item.offset + 1), validTerminals)
        }
      )
    }

    for nextSymbol in Set(iset.keys.compactMap { $0.next }) {
      let nextID = fn(closure(itemSetFor(next: nextSymbol)))
      transitions.shift[nextSymbol] = nextID
    }

    for (item, terminals) in iset {
      if item.next == nil {
        for t in terminals {
          if let existing = transitions.reduce[t] {
            throw GrammarError.reduceReduce(t, existing, item.rule)
          }
          if case .terminal(let terminal) = t {
            if transitions.shift[.terminal(terminal)] != nil {
              let badItems: [Item] = iset.keys.compactMap { item in
                if item.next != .terminal(terminal) {
                  nil
                } else {
                  item
                }
              }
              throw GrammarError.shiftReduce(terminal, item, badItems)
            }
          }
          transitions.reduce[t] = item.rule
        }
      }
    }

    return transitions
  }
}
