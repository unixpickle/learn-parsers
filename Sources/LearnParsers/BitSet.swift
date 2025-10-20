public struct BitSet: Hashable, Sendable, Codable {
  var cells: [UInt64]

  var count: Int {
    var result = 0
    for c in cells {
      for i in 0..<64 {
        if (c & (UInt64(1) << i)) != 0 {
          result += 1
        }
      }
    }
    return result
  }

  public init(count: Int) {
    let cellCount = (count + 63) / 64
    cells = [UInt64](repeating: 0, count: cellCount)
  }

  public subscript(_ i: Int) -> Bool {
    get {
      let cellIdx = i / 64
      let cellBit = (i % 64)
      return (cells[cellIdx] & (UInt64(1) << cellBit)) != 0
    }
    set {
      let cellIdx = i / 64
      let cellBit = (i % 64)
      let mask = (UInt64(1) << cellBit)
      if newValue {
        cells[cellIdx] |= mask
      } else {
        cells[cellIdx] &= ~mask
      }
    }
  }

  public mutating func formUnion(_ other: BitSet) {
    for i in 0..<cells.count {
      cells[i] |= other.cells[i]
    }
  }

  public mutating func union(_ other: BitSet) -> BitSet {
    var result = self
    result.formUnion(other)
    return result
  }
}

public struct BitSetConverter<T: Hashable>: Sendable where T: Sendable {
  typealias T = T

  public let valueToID: [T: Int]
  public let idToValue: [Int: T]

  public init(values: some Sequence<T>) {
    valueToID = Dictionary(uniqueKeysWithValues: values.enumerated().map { ($0.1, $0.0) })
    idToValue = Dictionary(uniqueKeysWithValues: values.enumerated().map { ($0.0, $0.1) })
  }

  public func empty() -> BitSet {
    BitSet(count: valueToID.count)
  }

  public func toBits(_ set: some Sequence<T>) -> BitSet {
    var result = BitSet(count: valueToID.count)
    for x in set {
      result[valueToID[x]!] = true
    }
    return result
  }

  public func fromBits(_ set: BitSet) -> OrderedSet<T> {
    var result = OrderedSet<T>()
    for id in 0..<idToValue.count {
      if set[id] {
        result.insert(idToValue[id]!)
      }
    }
    return result
  }

  public func toID(_ x: T) -> Int {
    valueToID[x]!
  }
}

extension BitSetConverter: Codable where BitSetConverter.T: Codable {}
