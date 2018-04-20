//
//  Flipper.swift
//  Flipper
//
//  Created by Harlan Haskins on 2/9/18.
//

import Foundation
import Clibflipper

public struct Flipper {
  let device: UnsafeMutablePointer<_lf_device>

  public static func attach() -> Flipper? {
    guard let device = flipper_attach() else { return nil}
    return Flipper(device: device)
  }

  public static func attachHostname(_ hostname: String) -> Flipper? {
    return hostname.withCString {
      let mutPtr = UnsafeMutablePointer(mutating: $0)
      guard let device = carbon_attach_hostname(mutPtr) else { return nil}
      return Flipper(device: device)
    }
  }
}

public struct LED: StandardModule {
  public static let underlyingModule = _led

  let ffi: ModuleFFI

  public init(ffi: ModuleFFI) {
    self.ffi = ffi
  }

  public func configure() {
    ffi.invoke(index: 0, args: [])
  }

  public func rgb(_ r: UInt8, _ g: UInt8, _ b: UInt8) {
    ffi.invoke(index: 1, args: [r, g, b])
  }
}
