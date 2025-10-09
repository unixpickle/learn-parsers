public struct OrderedSet<T: Hashable>: Hashable, Sequence, ExpressibleByArrayLiteral,
  CustomStringConvertible
{
  public typealias ArrayLiteralElement = T

  private var s = Set<T>()
  private var a = [T]()

  public var count: Int { s.count }
  public var isEmpty: Bool { s.isEmpty }

  public var description: String {
    "OrderedSet(\(a))"
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(s)
  }

  public static func == (lhs: OrderedSet<T>, rhs: OrderedSet<T>) -> Bool {
    lhs.s == rhs.s
  }

  public init() {
  }

  public init(arrayLiteral arr: T...) {
    s = Set(arr)
    a = arr
  }

  public init(_ arr: some Sequence<T>) {
    a = Array(arr)
    s = Set(a)
  }

  @discardableResult
  public mutating func insert(_ x: T) -> Bool {
    if s.insert(x).inserted {
      a.append(x)
      return true
    } else {
      return false
    }
  }

  public mutating func remove(_ x: T) {
    if s.remove(x) != nil {
      a.remove(at: a.firstIndex(of: x)!)
    }
  }

  public func contains(_ x: T) -> Bool {
    return s.contains(x)
  }

  public func makeIterator() -> IndexingIterator<[T]> {
    return a.makeIterator()
  }

  public func union(_ other: some Sequence<T>) -> OrderedSet<T> {
    var result = self
    result.formUnion(other)
    return result
  }

  @discardableResult
  public mutating func formUnion(_ other: some Sequence<T>) -> Int {
    var n = 0
    for x in other {
      if insert(x) {
        n += 1
      }
    }
    return n
  }

  public mutating func subtracting(_ other: some Sequence<T>) -> OrderedSet<T> {
    var result = self
    result.subtract(other)
    return result
  }

  public mutating func subtract(_ other: some Sequence<T>) {
    for x in other {
      remove(x)
    }
  }
}

extension OrderedSet: Sendable where OrderedSet.Element: Sendable {}

struct OrderedDict<K: Hashable, V: Hashable>: Hashable, Sequence, ExpressibleByDictionaryLiteral {
  private var d: [K: V] = [:]
  private var keyArr = [K]()

  public var count: Int { d.count }
  public var isEmpty: Bool { d.isEmpty }
  public var keys: [K] { keyArr }
  public var values: [V] { keyArr.map { d[$0]! } }
  public var first: (K, V)? {
    if let first = keyArr.first {
      (first, d[first]!)
    } else {
      nil
    }
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(d)
  }

  public static func == (lhs: OrderedDict<K, V>, rhs: OrderedDict<K, V>) -> Bool {
    lhs.d == rhs.d
  }

  public init() {
  }

  public init(uniqueKeysWithValues x: some Sequence<(K, V)>) {
    for (k, v) in x {
      self[k] = v
    }
  }

  public init(dictionaryLiteral elements: (K, V)...) {
    for (k, v) in elements {
      self[k] = v
    }
  }

  public subscript(_ k: K) -> V? {
    get {
      d[k]
    }
    set {
      if d[k] == nil {
        keyArr.append(k)
      }
      d[k] = newValue
    }
  }

  public subscript(_ k: K, default de: V) -> V {
    d[k, default: de]
  }

  public func makeIterator() -> IndexingIterator<[(K, V)]> {
    keyArr.map { ($0, d[$0]!) }.makeIterator()
  }
}

extension OrderedDict: Sendable where OrderedDict.Key: Sendable, OrderedDict.Value: Sendable {}
