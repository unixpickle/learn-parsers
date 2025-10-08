import Testing

@testable import LearnParsers

@Test func testGLRWikipediaGrammar() async throws {
  // Example grammar from wikipedia which is ambiguous
  let grammar = try StringGrammar(
    start: "S",
    "S -> E",
    "E -> T | ( E )",
    "T -> n | + T | T + n"
  )
  let parser = try GLRParser(grammar: grammar)

  var p = parser
  let match0 = try await p.read(StringParserReader("(n+n)"))
  #expect(matchToString(match0) == "(n+n)")

  p = parser
  let match1 = try await p.read(StringParserReader("+n+n"))
  #expect(matchToString(match1) == "+n+n")

  p = parser
  let match2 = try await p.read(StringParserReader("(+n+n+n+n+n)"))
  #expect(matchToString(match2) == "(+n+n+n+n+n)")

  p = parser
  let match3 = try await p.read(StringParserReader("n+n+n+n"))
  #expect(matchToString(match3) == "n+n+n+n")

  await #expect(throws: ParserReadError.self) {
    p = parser
    let _ = try await p.read(StringParserReader("n+n+n+"))
  }

  await #expect(throws: ParserReadError.self) {
    p = parser
    let _ = try await p.read(StringParserReader("++n+n+n+"))
  }

  await #expect(throws: ParserReadError.self) {
    p = parser
    let _ = try await p.read(StringParserReader("abc"))
  }
}

@Test func testGLRBranching() async throws {
  // Example grammar from wikipedia which is ambiguous
  let grammar = try StringGrammar(
    start: "S",
    "S -> A | B | S S",
    "A -> x | y",
    "B -> y | z"
  )
  let parser = try GLRParser(grammar: grammar)

  var p = parser
  let match0 = try await p.read(StringParserReader("xxyyzz"))
  #expect(matchToString(match0) == "xxyyzz")

  p = parser
  let match1 = try await p.read(StringParserReader("xxyyzzxyz"))
  #expect(matchToString(match1) == "xxyyzzxyz")

  // Matches should be deterministic even though they are arbitrary.
  for _ in 0..<5 {
    p = try GLRParser(grammar: grammar)
    let match2 = try await p.read(StringParserReader("xxyyzzxyz"))
    #expect(match1 == match2)
  }
}

@Test func testGLRLongLookahead() async throws {
  // Example grammar from wikipedia which is ambiguous
  let grammar = try StringGrammar(
    start: "S",
    "S -> A B C 1 | B A C 2 | C B A 3",
    "A -> x",
    "B -> x",
    "C -> x"
  )
  let parser = try GLRParser(grammar: grammar)

  var p = parser
  let match0 = try await p.read(StringParserReader("xxx1"))
  #expect(
    match0
      == .nonTerminal(
        lhs: "S",
        rhs: [
          .nonTerminal(lhs: "A", rhs: [.terminal("x")]),
          .nonTerminal(lhs: "B", rhs: [.terminal("x")]),
          .nonTerminal(lhs: "C", rhs: [.terminal("x")]),
          .terminal("1"),
        ])
  )

  p = parser
  let match1 = try await p.read(StringParserReader("xxx3"))
  #expect(
    match1
      == .nonTerminal(
        lhs: "S",
        rhs: [
          .nonTerminal(lhs: "C", rhs: [.terminal("x")]),
          .nonTerminal(lhs: "B", rhs: [.terminal("x")]),
          .nonTerminal(lhs: "A", rhs: [.terminal("x")]),
          .terminal("3"),
        ])
  )
}

@Test func testGLRInfiniteLoop() async throws {
  // Example grammar from wikipedia which is ambiguous
  let grammar = try StringGrammar(
    start: "S",
    "S -> A B x",
    "A -> A A | y | ",
    "B -> B B | y | "
  )
  let parser = try GLRParser(grammar: grammar)

  var p = parser
  let match0 = try await p.read(StringParserReader("x"))
  #expect(matchToString(match0) == "x")

  p = parser
  let match1 = try await p.read(StringParserReader("yyyyx"))
  #expect(matchToString(match1) == "yyyyx")
}

@Test func testGLRInfiniteLoop2() async throws {
  // Example grammar from wikipedia which is ambiguous
  let grammar = try StringGrammar(
    start: "S",
    "S -> A B x",
    "A -> B A | y | ",
    "B -> A B | y | ",
    "C -> A B | C A B | y | "
  )
  let parser = try GLRParser(grammar: grammar)

  var p = parser
  let match0 = try await p.read(StringParserReader("x"))
  #expect(matchToString(match0) == "x")

  p = parser
  let match1 = try await p.read(StringParserReader("yyyyx"))
  #expect(matchToString(match1) == "yyyyx")
}

func matchToString(_ m: ParserMatch<String, String>) -> String {
  switch m {
  case .terminal(let x): x
  case .nonTerminal(_, let rhs): rhs.map(matchToString).joined()
  }
}
