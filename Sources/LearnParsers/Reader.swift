import Foundation

public protocol ParserReader {
  associatedtype Terminal: Hashable

  func readTerminal() async -> Result<Terminal?, Error>
}

public class DataBytesParserReader: ParserReader {
  public typealias Terminal = UInt8

  public let bytes: Data
  private var offset: Int = 0

  public init(_ data: Data) {
    self.bytes = data
  }

  public func readTerminal() async -> Result<Terminal?, Error> {
    if offset == bytes.count {
      return .success(nil)
    } else {
      let result = bytes[offset]
      offset += 1
      return .success(result)
    }
  }
}
