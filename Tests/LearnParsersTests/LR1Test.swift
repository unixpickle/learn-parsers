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

@Test func testLR1EmptyTokenMiddle() async throws {
  let grammar = try StringGrammar(
    start: "S",
    "S -> A B x",
    "A -> y",
    "B -> z | "
  )
  let parser = try LR1Parser(grammar: grammar)

  var parser1 = parser
  let matches = try await parser1.read(StringParserReader("yx"))
  let expected: ParserMatch = .nonTerminal(
    lhs: "S",
    rhs: [
      .nonTerminal(lhs: "A", rhs: [.terminal("y")]),
      .nonTerminal(lhs: "B", rhs: []),
      .terminal("x"),
    ]
  )
  #expect(matches == expected)
}

@Test func testLR1EmptyTokenEnd() async throws {
  let grammar = try StringGrammar(
    start: "S",
    "S -> x A B C",
    "A -> y",
    "B -> z | ",
    "C -> w | "
  )
  var parser = try LR1Parser(grammar: grammar)
  let matches = try await parser.read(StringParserReader("xy"))
  let expected: ParserMatch = .nonTerminal(
    lhs: "S",
    rhs: [
      .terminal("x"),
      .nonTerminal(lhs: "A", rhs: [.terminal("y")]),
      .nonTerminal(lhs: "B", rhs: []),
      .nonTerminal(lhs: "C", rhs: []),
    ]
  )
  #expect(matches == expected)
}

@Test func testLR1ManyEmpty() async throws {
  let grammar = try StringGrammar(
    start: "D",
    "S -> A B C",
    "A -> a | ",
    "B -> b | ",
    "C -> c | ",
    "D -> S d e"
  )
  let parser = try LR1Parser(grammar: grammar)

  try await {
    var parser = parser
    let matches = try await parser.read(StringParserReader("cde"))
    let expected: ParserMatch = .nonTerminal(
      lhs: "D",
      rhs: [
        .nonTerminal(
          lhs: "S",
          rhs: [
            .nonTerminal(lhs: "A", rhs: []),
            .nonTerminal(lhs: "B", rhs: []),
            .nonTerminal(lhs: "C", rhs: [.terminal("c")]),
          ]
        ),
        .terminal("d"),
        .terminal("e"),
      ]
    )
    #expect(matches == expected)
  }()

  try await {
    var parser = parser
    let matches = try await parser.read(StringParserReader("de"))
    let expected: ParserMatch = .nonTerminal(
      lhs: "D",
      rhs: [
        .nonTerminal(
          lhs: "S",
          rhs: [
            .nonTerminal(lhs: "A", rhs: []),
            .nonTerminal(lhs: "B", rhs: []),
            .nonTerminal(lhs: "C", rhs: []),
          ]
        ),
        .terminal("d"),
        .terminal("e"),
      ]
    )
    #expect(matches == expected)
  }()

  try await {
    var parser = parser
    let matches = try await parser.read(StringParserReader("ade"))
    let expected: ParserMatch = .nonTerminal(
      lhs: "D",
      rhs: [
        .nonTerminal(
          lhs: "S",
          rhs: [
            .nonTerminal(lhs: "A", rhs: [.terminal("a")]),
            .nonTerminal(lhs: "B", rhs: []),
            .nonTerminal(lhs: "C", rhs: []),
          ]
        ),
        .terminal("d"),
        .terminal("e"),
      ]
    )
    #expect(matches == expected)
  }()
}
