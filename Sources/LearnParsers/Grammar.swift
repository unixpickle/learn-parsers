public struct Grammar<Terminal: Hashable, NonTerminal: Hashable> {
  public enum Symbol {
    case terminal(Terminal)
    case nonTerminal(NonTerminal)
  }

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
