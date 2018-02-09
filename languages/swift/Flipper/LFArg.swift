import CFlipper

public protocol LFArg {
  var asLFArg: _lf_arg { get }
}

public enum LFType: UInt8 {
  /* unsigned types */
  case u8 = 0
  case u16 = 1
  case u32 = 3
  case u64 = 7

  /* signed types */
  case i8 = 8
  case i16 = 9
  case i32 = 11
  case i64 = 15

  /* misc types */
  case void = 2
  case int = 4
  case ptr = 6
}

extension SignedInteger {
  var toLFArg: lf_arg {
    return lf_arg(bitPattern: Int64(truncatingIfNeeded: self))
  }
}

extension Int: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.int.rawValue,
                   value: toLFArg)
  }
}

extension Int8: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.i8.rawValue,
                   value: toLFArg)
  }
}

extension Int16: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.i16.rawValue,
                   value: toLFArg)
  }
}

extension Int32: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.i32.rawValue,
                   value: toLFArg)
  }
}

extension Int64: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.i64.rawValue,
                   value: toLFArg)
  }
}

extension UInt: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.u64.rawValue,
                   value: UInt64(self))
  }
}

extension UInt8: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.u8.rawValue,
                   value: UInt64(self))
  }
}

extension UInt16: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.u16.rawValue,
                   value: UInt64(self))
  }
}

extension UInt32: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.u32.rawValue,
                   value: UInt64(self))
  }
}

extension UInt64: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.u64.rawValue,
                   value: self)
  }
}

extension DevicePointer: LFArg {
  public var asLFArg: _lf_arg {
    return _lf_arg(type: LFType.ptr.rawValue, value: bitPattern)
  }
}
