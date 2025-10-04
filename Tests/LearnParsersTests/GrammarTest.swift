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
    grammar.firstTerminals() == [
      "S": [.terminal("n"), .terminal("+"), .terminal("(")],
      "E": [.terminal("n"), .terminal("+"), .terminal("(")],
      "T": [.terminal("n"), .terminal("+")],
    ]
  )
}

@Test func testFirstTerminalsEmpty() async throws {
  let grammar = try StringGrammar(
    start: "D",
    "S -> A B C",
    "A -> a | ",
    "B -> b | ",
    "C -> c | ",
    "D -> S d e"
  )
  #expect(
    grammar.firstTerminals() == [
      "S": [.terminal("a"), .terminal("b"), .terminal("c"), .end],
      "A": [.terminal("a"), .end],
      "B": [.terminal("b"), .end],
      "C": [.terminal("c"), .end],
      "D": [.terminal("a"), .terminal("b"), .terminal("c"), .terminal("d")],
    ]
  )
}
