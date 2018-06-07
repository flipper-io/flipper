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

public struct LED {
  let module: Module

  public init(flipper: Flipper) {
    self.module = Module(name: "led", device: flipper)
  }

  public func rgb(_ r: UInt8, _ g: UInt8, _ b: UInt8) throws {
    try module.invoke(index: 0, args: [r, g, b])
  }

  public func configure() throws {
    try module.invoke(index: 1, args: [])
  }
}
