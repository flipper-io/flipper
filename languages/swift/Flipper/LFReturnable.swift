//
//  LFReturnable.swift
//  Flipper
//
//  Created by Harlan Haskins on 1/31/18.
//

import Foundation
import CFlipper

public protocol LFReturnable {
  static var lfType: LFType { get }
  init(lfReturn: lf_return_t)
}

// Empty marker type to represent a Void return.
struct LFVoid: LFReturnable {
  static var lfType: LFType { return .void }
  init() {}
  init(lfReturn: lf_return_t) { self = LFVoid() }
}

extension Int: LFReturnable {
  public static var lfType: LFType {
    return .int
  }

  public init(lfReturn: lf_return_t) {
    self = Int(bitPattern: UInt(truncatingIfNeeded: lfReturn))
  }
}

extension Int8: LFReturnable {
  public static var lfType: LFType {
    return .int
  }

  public init(lfReturn: lf_return_t) {
    self = Int8(bitPattern: UInt8(truncatingIfNeeded: lfReturn))
  }
}

extension Int16: LFReturnable {
  public static var lfType: LFType {
    return .int
  }

  public init(lfReturn: lf_return_t) {
    self = Int16(bitPattern: UInt16(truncatingIfNeeded: lfReturn))
  }
}

extension Int32: LFReturnable {
  public static var lfType: LFType {
    return .int
  }

  public init(lfReturn: lf_return_t) {
    self = Int32(bitPattern: lfReturn)
  }
}

extension Int64: LFReturnable {
  public static var lfType: LFType {
    return .int
  }

  public init(lfReturn: lf_return_t) {
    self = Int64(bitPattern: UInt64(lfReturn))
  }
}

extension UInt: LFReturnable {
  public static var lfType: LFType {
    #if arch(x86_64) || arch(arm64)
    return .u64
    #else
    return .u32
    #endif
  }
  public init(lfReturn: lf_return_t) {
    self = UInt(truncatingIfNeeded: lfReturn)
  }
}

extension UInt8: LFReturnable {
  public static var lfType: LFType {
    return .u8
  }
  public init(lfReturn: lf_return_t) {
    self = UInt8(truncatingIfNeeded: lfReturn)
  }
}

extension UInt16: LFReturnable {
  public static var lfType: LFType {
    return .u16
  }
  public init(lfReturn: lf_return_t) {
    self = UInt16(truncatingIfNeeded: lfReturn)
  }
}

extension UInt32: LFReturnable {
  public static var lfType: LFType {
    return .u32
  }
  public init(lfReturn: lf_return_t) {
    self = UInt32(lfReturn)
  }
}

extension UInt64: LFReturnable {
  public static var lfType: LFType {
    return .u64
  }
  public init(lfReturn: lf_return_t) {
    self = UInt64(lfReturn)
  }
}

extension UnsafeRawPointer: LFReturnable {
  public static var lfType: LFType {
    return .ptr
  }
  public init(lfReturn: lf_return_t) {
    // FIXME: Handle `null`.
    self = UnsafeRawPointer(bitPattern:
      UInt(truncatingIfNeeded: lfReturn))!
  }
}

extension UnsafeMutableRawPointer: LFReturnable {
  public static var lfType: LFType {
    return .ptr
  }
  public init(lfReturn: lf_return_t) {
    // FIXME: Handle `null`.
    self = UnsafeMutableRawPointer(bitPattern:
      UInt(truncatingIfNeeded: lfReturn))!
  }
}
