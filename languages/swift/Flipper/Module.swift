import Foundation
import Clibflipper

public struct FlipperError: Error, CustomStringConvertible {
  public let message: String

  static var current: FlipperError? {
    if lf_error_get() == E_OK {
      return nil
    }
    return FlipperError(message: String(cString: lf_error_string()))
  }

  public var description: String {
    return message
  }
}

public struct Module {
  let device: UnsafeMutablePointer<_lf_device>?
  let name: String

  init(name: String, device: Flipper) {
    self.name = name
    self.device = device.device
  }

  public func invoke(index: UInt8, args: [LFArg]) throws {
    _ = try invoke(index: index, args: args) as LFVoid
  }

  public func invoke<Ret: LFReturnable>(
    index: UInt8,
    args: [LFArg]
  ) throws -> Ret {
    let ret = name.withCString { bytes -> lf_return_t in
      let mutPtr = UnsafeMutablePointer(mutating: bytes)
      var ret = lf_return_t()
      lf_invoke(device, mutPtr, index, Ret.lfType.rawValue,
                &ret, buildLinkedList(args))
      return ret
    }
    if let err = FlipperError.current {
      throw err
    }
    return Ret.init(lfReturn: ret)
  }

  public func push(
    index: UInt8,
    data: Data,
    destination: DevicePointer
  ) throws {
    let ptr = UnsafeMutableRawPointer(bitPattern: UInt(destination.bitPattern))
    data.withUnsafeBytes { (bytes: UnsafePointer<UInt8>) -> Void in
      let rawPtr = UnsafeMutableRawPointer(mutating: bytes)
      lf_push(device, ptr, rawPtr, data.count)
      return
    }
    if let err = FlipperError.current {
      throw err
    }
  }

  public func pull(from source: DevicePointer, byteCount: Int) throws -> Data {
    var resultData = Data(repeating: 0, count: byteCount)
    let ptr = UnsafeMutableRawPointer(bitPattern: UInt(source.bitPattern))
    resultData.withUnsafeMutableBytes {
      (bytes: UnsafeMutablePointer<UInt8>) -> Void in
      lf_pull(device, bytes, ptr, byteCount)
      return
    }
    if let err = FlipperError.current {
      throw err
    }
    return resultData
  }
}

func buildLinkedList(_ args: [LFArg]) -> UnsafeMutablePointer<_lf_ll>? {
  var argList: UnsafeMutablePointer<_lf_ll>? = nil
  for arg in args {
    lf_ll_append(&argList,
                 UnsafeMutableRawPointer(bitPattern:
                  UInt(truncatingIfNeeded: arg.asLFArg.value)), nil)
  }
  return argList
}
