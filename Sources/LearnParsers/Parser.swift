public enum ParserMatch<Terminal: SymbolProto, NonTerminal: SymbolProto>: Hashable, Sendable,
  CustomStringConvertible
{
  case terminal(Terminal)
  indirect case nonTerminal(lhs: NonTerminal, rhs: [Self])

  public var description: String {
    switch self {
    case .terminal(let t): "terminal(\(t))"
    case .nonTerminal(let lhs, let rhs): "nonTerminal(\(lhs), \(rhs))"
    }
  }
}

public protocol Parser {
  associatedtype Terminal: SymbolProto
  associatedtype NonTerminal: SymbolProto

  typealias Symbol = GrammarSymbol<Terminal, NonTerminal>
  typealias Match = ParserMatch<Terminal, NonTerminal>

  mutating func put(terminal: Terminal) throws
  mutating func end() throws -> Match
}

public struct ParserReadError: Error {
  let metadata: TokenMetadata
  let error: Error
}

extension Parser {
  mutating public func read<T: ParserReader>(_ reader: T) async throws -> Match
  where T.Terminal == Terminal {
    while true {
      let (token, metadata) = try await reader.readTerminal()
      do {
        if let token = token {
          try put(terminal: token)
        } else {
          return try end()
        }
      } catch {
        throw ParserReadError(metadata: metadata, error: error)
      }
    }
  }
}
