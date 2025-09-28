public protocol Parser {
  associatedtype Terminal: SymbolProto
  associatedtype NonTerminal: SymbolProto

  typealias Symbol = GrammarSymbol<Terminal, NonTerminal>

  func put(terminal: Terminal) throws
  func end() throws -> Symbol
}

public struct ParserReadError: Error {
  let metadata: TokenMetadata
  let error: Error
}

extension Parser {
  public func read<T: ParserReader>(_ reader: T) async throws -> Symbol
  where T.Terminal == Terminal {
    while let (token, metadata) = try await reader.readTerminal() {
      do {
        try put(terminal: token)
      } catch {
        throw ParserReadError(metadata: metadata, error: error)
      }
    }
    return try end()
  }
}
