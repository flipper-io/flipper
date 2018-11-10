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

extension Flipper {
    public var led: LED {
        return LED(flipper: self)
    }
}
