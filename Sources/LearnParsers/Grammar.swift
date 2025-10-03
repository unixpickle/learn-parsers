public protocol SymbolProto: Hashable, Sendable {}

extension String: SymbolProto {}

public enum GrammarSymbol<Terminal: SymbolProto, NonTerminal: SymbolProto>: Hashable, Sendable,
  CustomStringConvertible
{
  case terminal(Terminal)
  case nonTerminal(NonTerminal)

  public var description: String {
    switch self {
    case .terminal(let t):
      return "terminal(\(t))"
    case .nonTerminal(let nt):
      return "nonTerminal⟨\(nt)⟩"
    }
  }
}

public class Grammar<Terminal: SymbolProto, NonTerminal: SymbolProto> {
  public typealias Symbol = GrammarSymbol<Terminal, NonTerminal>

  public struct Rule: Hashable, Sendable, CustomStringConvertible {
    public let lhs: NonTerminal
    public let rhs: [Symbol]

    public var description: String {
      "Rule(\(lhs) -> \(rhs.map{ $0.description }.joined(separator: " ")))"
    }

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
  public enum ParseError: Error {
    case invalidNumberOfArrows
  }

  convenience public init(start: String, _ rules: String...) throws {
    var lhsAndRhs = [(String, [String])]()
    for line in rules {
      let parts = line.split(separator: "->")
      if parts.count != 2 {
        throw ParseError.invalidNumberOfArrows
      }
      let lhs = String(parts[0].trimmingCharacters(in: .whitespaces))
      let rhses = parts[1].split(separator: "|")
      for rhs in rhses {
        if rhs.trimmingCharacters(in: .whitespaces).isEmpty {
          lhsAndRhs.append((lhs, []))
        } else {
          let rhsComponents = rhs.split(separator: " ").map(String.init)
          lhsAndRhs.append((lhs, rhsComponents))
        }
      }
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
