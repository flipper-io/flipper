import Flipper
import Foundation

guard let device = Flipper.attach() else {
    fatalError("Could not attach to fvm")
}

let led = LED(flipper: device)

public class PressRecognizer {
    public enum State {
        case idle
        case pressed(start: CFAbsoluteTime)
    }

    public typealias CallbackFunction = () throws -> Void

    private let button: Button
    private let holdTimeout = 0.5

    private var state = State.idle

    private var holdTriggered = false

    private var stateChangeCallback: ((State) throws -> Void)?
    private var onHoldCallback: CallbackFunction?
    private var onPressCallback: CallbackFunction?
    private var onReleaseCallback: CallbackFunction?

    public init(flipper: Flipper) {
        self.button = flipper.button
    }

    public func onHold(_ fn: CallbackFunction?) {
        onHoldCallback = fn
    }

    public func onPress(_ fn: CallbackFunction?) {
        onPressCallback = fn
    }

    public func onRelease(_ fn: CallbackFunction?) {
        onReleaseCallback = fn
    }

    public func onStateChange(_ fn: ((State) throws -> Void)?) {
        stateChangeCallback = fn
    }

    public func update() throws {
        switch (state, try button.isPressed()) {
        case (.idle, false):
            break
        case (.idle, true):
            state = .pressed(start: CFAbsoluteTimeGetCurrent())
            try stateChangeCallback?(state)
            try onPressCallback?()
        case (.pressed, false):
            state = .idle
            try stateChangeCallback?(state)
            holdTriggered = false
            try onReleaseCallback?()
        case (.pressed(let time), true):
            let currentTime = CFAbsoluteTimeGetCurrent()
            if !holdTriggered && currentTime - time > holdTimeout {
                try onHoldCallback?()
                holdTriggered = true
            }
        }
    }
}

let pressRecognizer = PressRecognizer(flipper: device)
pressRecognizer.onPress {
    try device.led.rgb(10, 0, 0)
}

pressRecognizer.onStateChange {
    print("state: \($0)")
}

pressRecognizer.onRelease {
    try device.led.rgb(0, 10, 0)
}

while true {
    try pressRecognizer.update()
}
