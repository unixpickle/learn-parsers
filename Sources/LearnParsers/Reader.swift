import Foundation

public struct TokenMetadata: Sendable, Hashable {
  let line: Int?
  let column: Int?
}

public protocol ParserReader {
  associatedtype Terminal: Hashable

  func readTerminal() async throws -> (Terminal, TokenMetadata)?
}

public class DataBytesParserReader: ParserReader {
  public typealias Terminal = UInt8

  public let bytes: Data
  private var offset: Int = 0
  private var line: Int = 0
  private var column: Int = 0

  public init(_ data: Data) {
    self.bytes = data
  }

  public func readTerminal() async throws -> (Terminal, TokenMetadata)? {
    if offset == bytes.count {
      return nil
    } else {
      let result = bytes[offset]
      offset += 1
      let metadata = TokenMetadata(line: line, column: column)
      if result == "\n".utf8.first {
        line += 1
        column = 0
      } else {
        column += 1
      }
      return (result, metadata)
    }
  }
}
