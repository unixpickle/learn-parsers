public protocol SymbolProto: Hashable, Sendable {
}

public enum GrammarSymbol<Terminal: SymbolProto, NonTerminal: SymbolProto> {
  case terminal(Terminal)
  case nonTerminal(NonTerminal)
}

public struct Grammar<Terminal: SymbolProto, NonTerminal: SymbolProto> {
  public typealias Symbol = GrammarSymbol<Terminal, NonTerminal>

  public struct Rule {
    public let lhs: NonTerminal
    public let rhs: [Symbol]

    public init(lhs: NonTerminal, rhs: Symbol...) {
      self.lhs = lhs
      self.rhs = Array(rhs)
    }
  }

  public let rules: [Rule]

  public init(rules: [Rule]) {
    self.rules = rules
  }
}
