public struct Button {
    let module: Module

    public init(flipper: Flipper) {
        self.module = Module(name: "button", device: flipper)
    }

    public func isPressed() throws -> Bool {
        return try module.invoke(index: 0, args: [])
    }

    public func configure() throws {
        try module.invoke(index: 1, args: [])
    }
}

extension Flipper {
    public var button: Button {
        return Button(flipper: self)
    }
}
