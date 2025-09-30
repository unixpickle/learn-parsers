import Testing

@testable import LearnParsers

@Test func testFirstTerminalsSimple() async throws {
  let grammar = StringGrammar(
    "S E",
    "E T",
    "E ( E )",
    "T n",
    "T + T",
    "T T + n"
  )
  #expect(
    grammar.firstTerminals() == ["S": ["n", "+", "("], "E": ["n", "+", "("], "T": ["n", "+"]]
  )
}
