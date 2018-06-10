import Flipper

public class Test {
    public let description = ""
    let module: Module

    public init(flipper: Flipper) {
        self.module = Module(name: "Test", device: flipper)
    }


    public func test(a: Int, b: Character, c: Int64) throws -> Int {
        return try module.invoke(index: , args: [a, b, c])
    }

}