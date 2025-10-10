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

  public typealias G = G
  public typealias Terminal = Terminal
  public typealias NonTerminal = NonTerminal

  public typealias TerminalOrEnd = G.TerminalOrEnd
  public typealias Rule = G.Rule

  public final class RulePointer: Hashable, Sendable, CustomStringConvertible {
    public let rule: G.Rule

    public var lhs: NonTerminal { rule.lhs }
    public var rhs: [Symbol] { rule.rhs }
    public var description: String { rule.description }

    public init(rule: G.Rule) {
      self.rule = rule
    }

    public func hash(into hasher: inout Hasher) {
      hasher.combine(ObjectIdentifier(self))
    }

    public static func == (lhs: RulePointer, rhs: RulePointer) -> Bool {
      ObjectIdentifier(lhs) == ObjectIdentifier(rhs)
    }
  }

  public struct Item: Hashable, Sendable, CustomStringConvertible {
    let rule: RulePointer
    let offset: Int

    public var description: String {
      let pieces = rule.rhs.map { $0.description }
      let piecesWithDot = pieces[..<offset] + ["."] + pieces[offset...]
      return "Rule(\(rule.lhs) -> \(piecesWithDot.joined(separator: " ")))"
    }

    var next: Symbol? {
      offset < rule.rhs.count ? rule.rhs[offset] : nil
    }

    internal func nextTerminals(
      lookaheadTerminals: BitSet,
      firstTerminals: [NonTerminal: BitSet],
      bitSetConverter: BitSetConverter<TerminalOrEnd>
    ) -> BitSet {
      var possibleTerminals = bitSetConverter.empty()
      for i in (offset + 1)..<rule.rhs.count {
        switch rule.rhs[i] {
        case .terminal(let x):
          possibleTerminals[bitSetConverter.toID(.terminal(x))] = true
          return possibleTerminals
        case .nonTerminal(let x):
          let ft = firstTerminals[x, default: bitSetConverter.empty()]
          possibleTerminals.formUnion(ft)
          if !ft[bitSetConverter.toID(.end)] {
            return possibleTerminals
          }
        }
      }
      // It's possible that we reach the end.
      return possibleTerminals.union(lookaheadTerminals)
    }
  }

  /// Maps items to valid lookahead terminals.
  internal typealias ItemSet = OrderedDict<Item, BitSet>
  private typealias ItemSetID = Int

  private struct Transitions {
    public var shift: [Symbol: Int] = [:]
    public var reduce: [TerminalOrEnd: RulePointer] = [:]

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
  private let bitSetConverter: BitSetConverter<TerminalOrEnd>
  private let firstTerminals: [NonTerminal: BitSet]
  private var ruleMap = [NonTerminal: [RulePointer]]()
  private var itemSets = [ItemSet: ItemSetID]()
  private var transitionMap = [ItemSetID: Transitions]()

  private var stateStack: [ItemSetID] = []
  private var stateMatches: [Match] = []

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
    let seedItemSet: ItemSet = OrderedDict(
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
    precondition(stateStack.count > 0)
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
    Self.closure(
      iset, ruleMap: ruleMap,
      firstTerminals: firstTerminals,
      bitSetConverter: bitSetConverter
    )
  }

  internal static func closure(
    _ iset: ItemSet,
    ruleMap: [NonTerminal: [RulePointer]],
    firstTerminals: [NonTerminal: BitSet],
    bitSetConverter: BitSetConverter<TerminalOrEnd>
  ) -> ItemSet {
    var result = iset
    var checkItems = OrderedSet<Item>(iset.keys)
    while !checkItems.isEmpty {
      let ci = checkItems
      checkItems = .init()
      for sourceItem in ci {
        guard case .nonTerminal(let x) = sourceItem.next else {
          continue
        }
        let lookaheadTerminals = result[sourceItem]!
        let nextTerminals = sourceItem.nextTerminals(
          lookaheadTerminals: lookaheadTerminals,
          firstTerminals: firstTerminals,
          bitSetConverter: bitSetConverter
        )
        assert(lookaheadTerminals.count > 0)
        for expandRule in ruleMap[x, default: []] {
          let addedItem = Item(rule: expandRule, offset: 0)
          let hasNonTerminal = if case .nonTerminal = addedItem.next {
            true
          } else {
            false
          }
          if result[addedItem] == nil {
            result[addedItem] = nextTerminals
            if hasNonTerminal {
              checkItems.insert(addedItem)
            }
          } else {
            let oldResult = result[addedItem]!
            var newResult = oldResult
            newResult.formUnion(nextTerminals)
            if newResult != oldResult {
              result[addedItem] = newResult
              if hasNonTerminal {
                checkItems.insert(addedItem)
              }
            }
          }
        }
      }
    }
    return result
  }

  private func expandItemSet(_ iset: ItemSet, _ fn: (ItemSet) -> ItemSetID) throws -> Transitions {
    var transitions = Transitions(shift: [:], reduce: [:])

    var nextSymbols = Set<Symbol>()
    var nextSymbolsOrdered = [Symbol]()
    var nextItemSets = [Symbol: ItemSet]()
    for (item, validTerminals) in iset {
      guard let next = item.next else {
        continue
      }
      let newItem = Item(rule: item.rule, offset: item.offset + 1)
      if nextSymbols.insert(next).inserted {
        nextSymbolsOrdered.append(next)
      }
      nextItemSets[next, default: .init()][newItem] = validTerminals
    }

    for nextSymbol in nextSymbolsOrdered {
      let newItemSet = nextItemSets[nextSymbol]!
      let nextID = fn(closure(newItemSet))
      transitions.shift[nextSymbol] = nextID
    }

    for (item, terminals) in iset {
      if item.next == nil {
        for t in bitSetConverter.fromBits(terminals) {
          if let existing = transitions.reduce[t] {
            throw GrammarError.reduceReduce(t, existing.rule, item.rule.rule)
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

extension LR1Parser: Sendable where LR1Parser.G: Sendable {}
