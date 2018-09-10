@testable import Flipper
import Clibflipper

guard let device = Flipper.attach() else {
    fatalError("Could not attach to fvm")
}

device._selectU2()

let led = LED(flipper: device)
try led.rgb(10, 20, 30)
