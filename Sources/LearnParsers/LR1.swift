public enum LR1Error: Error {
  case notImplemented
}

public class LR1Parser<
  Terminal: SymbolProto, NonTerminal: SymbolProto, G: Grammar<Terminal, NonTerminal>
>: Parser {

  public enum GrammarError: Error {
    case reduceReduce(TerminalOrEnd, Rule, Rule)
    case shiftReduce(Terminal, Rule, [Rule])
  }

  public enum ParseError: Error {
    case unexpectedTerminal(TerminalOrEnd, [TerminalOrEnd])
  }

  public typealias Terminal = Terminal
  public typealias NonTerminal = NonTerminal

  public typealias Rule = G.Rule

  private struct Item: Hashable {
    let rule: G.Rule
    let offset: Int

    var next: Symbol? {
      offset < rule.rhs.count ? rule.rhs[offset] : nil
    }

    public func nextTerminals(
      lookaheadTerminals: Set<TerminalOrEnd>,
      firstTerminals: [NonTerminal: Set<Terminal>]
    ) -> Set<TerminalOrEnd> {
      precondition(offset < rule.rhs.count, "only call nextTerminals on items with a `next`")
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

  public enum TerminalOrEnd: Hashable, Sendable {
    case terminal(Terminal)
    case end
  }

  /// Maps items to valid lookahead terminals.
  private typealias ItemSet = [Item: Set<TerminalOrEnd>]
  private typealias ItemSetID = Int

  private struct Transitions {
    public var shift: [Symbol: Int] = [:]
    public var reduce: [TerminalOrEnd: Rule] = [:]
  }

  private let grammar: G
  private let firstTerminals: [NonTerminal: Set<Terminal>]
  private var ruleMap = [NonTerminal: [Rule]]()
  private var itemSets = [ItemSet: ItemSetID]()
  private var transitionMap = [ItemSetID: Transitions]()

  private var stateStack: [ItemSetID] = []
  private var stateMatches: [Match] = []

  public init(grammar: G, start: NonTerminal) throws {
    self.grammar = grammar
    self.firstTerminals = grammar.firstTerminals()

    for rule in grammar.rules {
      ruleMap[rule.lhs, default: []].append(rule)
    }

    let startItems = ruleMap[start, default: []].map { Item(rule: $0, offset: 0) }
    let seedItemSet: ItemSet = Dictionary(
      uniqueKeysWithValues: startItems.map { x in (x, [TerminalOrEnd.end]) }
    )
    let startItemSet = closure(seedItemSet)
    itemSets[startItemSet] = 0

    // Expand item sets for each symbol after a dot.
    var itemSetsToExpand = [startItemSet]
    while let itemSet = itemSetsToExpand.popLast() {
      transitionMap[itemSets[itemSet]!] = try expandItemSet(itemSet) { newItemSet in
        if let id = itemSets[newItemSet] {
          return id
        } else {
          let id = itemSets.count
          itemSets[newItemSet] = id
          itemSetsToExpand.append(newItemSet)
          return id
        }
      }
    }

    stateStack.append(0)
  }

  public func put(terminal: Terminal) throws {
    try put(.terminal(terminal))
  }

  public func end() throws -> Match {
    try put(.end)
    assert(stateMatches.count == 1)
    return stateMatches.first!
  }

  private func put(_ t: TerminalOrEnd) throws {
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
        guard let shift = newMap.shift[.nonTerminal(reduce.lhs)] else {
          fatalError("matched rule but cannot shift it after popping stack")
        }
        stateStack.append(shift)
      } else {
        throw ParseError.unexpectedTerminal(
          t,
          map.shift.keys.compactMap { symbol in
            if case .terminal(let t) = symbol {
              .terminal(t)
            } else {
              nil
            }
          } + map.reduce.keys
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
          if item.next != next || item.offset + 1 == item.rule.rhs.count {
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
              throw GrammarError.shiftReduce(
                terminal, item.rule, itemSetFor(next: .terminal(terminal)).keys.map { $0.rule }
              )
            }
          }
          transitions.reduce[t] = item.rule
        }
      }
    }

    return transitions
  }
}
