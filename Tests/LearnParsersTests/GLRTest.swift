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

func matchToString(_ m: ParserMatch<String, String>) -> String {
  switch m {
  case .terminal(let x): x
  case .nonTerminal(_, let rhs): rhs.map(matchToString).joined()
  }
}
