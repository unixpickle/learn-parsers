public protocol SymbolProto: Hashable, Sendable {}

extension String: SymbolProto {}

public enum GrammarSymbol<Terminal: SymbolProto, NonTerminal: SymbolProto>: Hashable, Sendable {
  case terminal(Terminal)
  case nonTerminal(NonTerminal)
}

public class Grammar<Terminal: SymbolProto, NonTerminal: SymbolProto> {
  public typealias Symbol = GrammarSymbol<Terminal, NonTerminal>

  public struct Rule: Hashable, Sendable {
    public let lhs: NonTerminal
    public let rhs: [Symbol]

    public init(_ lhs: NonTerminal, _ rhs: Symbol...) {
      self.lhs = lhs
      self.rhs = Array(rhs)
    }

    public init(lhs: NonTerminal, rhs: [Symbol]) {
      self.lhs = lhs
      self.rhs = rhs
    }
  }

  public let start: NonTerminal
  public let rules: [Rule]

  public init(start: NonTerminal, rules: [Rule]) {
    self.start = start
    self.rules = rules
  }

  /// Find all of the passible terminals that may appear at the start of a
  /// non-terminal.
  public func firstTerminals() -> [NonTerminal: Set<Terminal>] {
    var mapping = [NonTerminal: Set<Terminal>]()
    var converged = true
    for rule in rules {
      if case .terminal(let x) = rule.rhs.first {
        mapping[rule.lhs, default: []].insert(x)
        converged = false
      }
    }
    while !converged {
      converged = true
      for rule in rules {
        guard case .nonTerminal(let x) = rule.rhs.first else {
          continue
        }
        guard let rhsStarts = mapping[x] else {
          continue
        }

        let oldCount = mapping[rule.lhs]?.count ?? 0
        let newSet = mapping[rule.lhs, default: []].union(rhsStarts)
        if newSet.count > oldCount {
          mapping[rule.lhs] = newSet
          converged = false
        }
      }
    }
    return mapping
  }
}

public class StringGrammar: Grammar<String, String> {
  convenience public init(start: String, _ rules: String...) {
    let lhsAndRhs = rules.map { line in
      let parts = line.split(separator: " ")
      precondition(parts.count > 1)
      return (String(parts[0]), parts[1...].map(String.init))
    }
    let nonTerminalStrings = Set(lhsAndRhs.map { $0.0 })
    self.init(
      start: start,
      rules: lhsAndRhs.map { rawRule in
        Rule(
          lhs: rawRule.0,
          rhs: rawRule.1.map { maybeTerminal in
            if nonTerminalStrings.contains(maybeTerminal) {
              Symbol.nonTerminal(maybeTerminal)
            } else {
              Symbol.terminal(maybeTerminal)
            }
          }
        )
      }
    )
  }
}
