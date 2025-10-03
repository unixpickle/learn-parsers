import Testing

@testable import LearnParsers

@Test func testLR1WikipediaGrammar() async throws {
  // Example grammar from wikipedia which is ambiguois
  let grammar = try StringGrammar(
    start: "S",
    "S -> E",
    "E -> T | ( E )",
    "T -> n | + T | T + n"
  )
  #expect(throws: LR1Parser<String, String, StringGrammar>.GrammarError.self) {
    _ = try LR1Parser(grammar: grammar)
  }
}

@Test func testLR1FixedWikipediaGrammar() async throws {
  // Example grammar from wikipedia with the tail factored out of T.
  let grammar = try StringGrammar(
    start: "S",
    "S -> E",
    "E -> T | ( E )",
    "T -> + T | n R",
    "R -> + n R | "
  )
  let parser = try LR1Parser(grammar: grammar)

  var parser1 = parser
  var matches = try await parser1.read(StringParserReader("(n+n)"))
  var expected: ParserMatch = .nonTerminal(
    lhs: "S",
    rhs: [
      .nonTerminal(
        lhs: "E",
        rhs: [
          .terminal("("),
          .nonTerminal(
            lhs: "E",
            rhs: [
              .nonTerminal(
                lhs: "T",
                rhs: [
                  .terminal("n"),
                  .nonTerminal(
                    lhs: "R",
                    rhs: [.terminal("+"), .terminal("n"), .nonTerminal(lhs: "R", rhs: [])]
                  ),
                ])
            ]
          ),
          .terminal(")"),
        ])
    ]
  )
  #expect(matches == expected)

  parser1 = parser
  matches = try await parser1.read(StringParserReader("n+n+n"))
  expected = .nonTerminal(
    lhs: "S",
    rhs: [
      .nonTerminal(
        lhs: "E",
        rhs: [
          .nonTerminal(
            lhs: "T",
            rhs: [
              .terminal("n"),
              .nonTerminal(
                lhs: "R",
                rhs: [
                  .terminal("+"), .terminal("n"),
                  .nonTerminal(
                    lhs: "R",
                    rhs: [
                      .terminal("+"),
                      .terminal("n"),
                      .nonTerminal(lhs: "R", rhs: []),
                    ]
                  ),
                ]
              ),
            ]
          )
        ]
      )
    ]
  )
  #expect(matches == expected)

  await #expect(throws: ParserReadError.self) {
    var parser1 = parser
    let _ = try await parser1.read(StringParserReader("(n+)"))
  }
  await #expect(throws: ParserReadError.self) {
    var parser1 = parser
    let _ = try await parser1.read(StringParserReader("(n+n"))
  }
  await #expect(throws: ParserReadError.self) {
    var parser1 = parser
    let _ = try await parser1.read(StringParserReader("(n+n+)"))
  }
  await #expect(throws: ParserReadError.self) {
    var parser1 = parser
    let _ = try await parser1.read(StringParserReader("+(n+n)"))
  }
  await #expect(throws: ParserReadError.self) {
    var parser1 = parser
    let _ = try await parser1.read(StringParserReader("((n+n)))"))
  }
}
