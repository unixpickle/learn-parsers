public enum LR1Error: Error {
  case notImplemented
}

public class LR1Parser<
  Terminal: SymbolProto, NonTerminal: SymbolProto, G: Grammar<Terminal, NonTerminal>
>: Parser {
  public typealias Terminal = Terminal
  public typealias NonTerminal = NonTerminal

  private typealias Rule = G.Rule

  private struct Item: Hashable {
    let rule: G.Rule
    let offset: Int

    var next: Symbol {
      rule.rhs[offset]
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

  private enum TerminalOrEnd: Hashable {
    case terminal(Terminal)
    case end
  }

  /// Maps items to valid lookahead terminals.
  private typealias ItemSet = [Item: Set<TerminalOrEnd>]

  private let grammar: G
  private let firstTerminals: [NonTerminal: Set<Terminal>]
  private var ruleMap = [NonTerminal: [Rule]]()
  private var itemSets = [ItemSet: Int]()

  private var stack: [ItemSet] = []

  public init(grammar: G, start: NonTerminal) {
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
      expandItemSet(itemSet) { newItemSet in
        if itemSets[newItemSet] == nil {
          itemSets[newItemSet] = itemSets.count
          itemSetsToExpand.append(newItemSet)
        }
      }
    }
  }

  public func put(terminal: Terminal) throws {
    throw LR1Error.notImplemented
  }

  public func end() throws -> Symbol {
    throw LR1Error.notImplemented
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

  private func expandItemSet(_ iset: ItemSet, _ fn: (ItemSet) -> Void) {
    for nextSymbol in Set(iset.keys.map { $0.next }) {
      let nextSet: ItemSet = Dictionary(
        uniqueKeysWithValues: iset.compactMap { (item, validTerminals) in
          if item.next != nextSymbol || item.offset + 1 == item.rule.rhs.count {
            return nil
          }
          return (Item(rule: item.rule, offset: item.offset + 1), validTerminals)
        }
      )
      fn(closure(nextSet))
    }
  }
}
