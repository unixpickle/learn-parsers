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

public enum GrammarTerminalOrEnd<Terminal: SymbolProto>: Hashable, Sendable, CustomStringConvertible
{
  case terminal(Terminal)
  case end

  public var description: String {
    switch self {
    case .terminal(let t): "terminal(\(t))"
    case .end: "end"
    }
  }
}

open class Grammar<Terminal: SymbolProto, NonTerminal: SymbolProto> {
  public typealias Symbol = GrammarSymbol<Terminal, NonTerminal>
  public typealias TerminalOrEnd = GrammarTerminalOrEnd<Terminal>

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

  /// Find all of the possible terminals that may appear at the start of a
  /// non-terminal, including .end if the non-terminal can be empty.
  public func firstTerminals() -> [NonTerminal: OrderedSet<TerminalOrEnd>] {
    var mapping = [NonTerminal: OrderedSet<TerminalOrEnd>]()
    var converged = true
    for rule in rules {
      if case .terminal(let x) = rule.rhs.first {
        mapping[rule.lhs, default: []].insert(.terminal(x))
        converged = false
      } else if rule.rhs.isEmpty {
        mapping[rule.lhs, default: []].insert(.end)
      }
    }

    while !converged {
      converged = true
      for rule in rules {
        // We have to scan through the rhs of the rule since some non-terminals
        // may be empty, in which case the first terminal might come after them.
        var canReachEnd = true
        var reachableTerminals = Set<TerminalOrEnd>()
        for symbol in rule.rhs {
          if case .nonTerminal(let x) = symbol {
            reachableTerminals.formUnion(mapping[x, default: []].subtracting([.end]))
            if !(mapping[x]?.contains(.end) ?? false) {
              canReachEnd = false
              break
            }
          } else if case .terminal(let x) = symbol {
            reachableTerminals.insert(.terminal(x))
            canReachEnd = false
            break
          }
        }
        if canReachEnd {
          reachableTerminals.insert(.end)
        }

        let oldSet = mapping[rule.lhs, default: []]
        let newSet = oldSet.union(reachableTerminals)
        if newSet.count > oldSet.count {
          mapping[rule.lhs] = newSet
          converged = false
        }
      }
    }
    return mapping
  }
}

open class StringGrammar: Grammar<String, String> {
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
