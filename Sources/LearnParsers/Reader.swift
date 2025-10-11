import Foundation

public struct TokenMetadata: Sendable, Hashable {
  public let line: Int?
  public let column: Int?

  public init(line: Int?, column: Int?) {
    self.line = line
    self.column = column
  }
}

public protocol ParserReader {
  associatedtype Terminal: Hashable

  func readTerminal() async throws -> (Terminal?, TokenMetadata)
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

  public func readTerminal() async throws -> (Terminal?, TokenMetadata) {
    let metadata = TokenMetadata(line: line, column: column)
    if offset == bytes.count {
      return (nil, metadata)
    } else {
      let result = bytes[offset]
      offset += 1
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

public class StringParserReader: ParserReader {
  public typealias Terminal = String

  private let text: String
  private var offset: String.Index
  private var line: Int = 0
  private var column: Int = 0

  public init(_ text: String) {
    self.text = text
    self.offset = text.startIndex
  }

  public func readTerminal() async throws -> (Terminal?, TokenMetadata) {
    let metadata = TokenMetadata(line: line, column: column)

    guard offset < text.endIndex else {
      return (nil, metadata)
    }

    let char = text[offset]
    let result = String(char)

    offset = text.index(after: offset)

    if char == "\n" {
      line += 1
      column = 0
    } else {
      column += 1
    }

    return (result, metadata)
  }
}
