import Testing

@testable import LearnParsers

@Test func testFirstTerminalsSimple() async throws {
  let grammar = try StringGrammar(
    start: "S",
    "S -> E",
    "E -> T | ( E )",
    "T -> n | + T | T + n"
  )
  #expect(
    grammar.firstTerminals() == ["S": ["n", "+", "("], "E": ["n", "+", "("], "T": ["n", "+"]]
  )
}
