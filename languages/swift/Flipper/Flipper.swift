import Foundation
import Clibflipper

public struct Flipper {
    let device: UnsafeMutablePointer<_lf_device>
    
    public static func attach() -> Flipper? {
        guard carbon_attach() != nil else { return nil }
        return Flipper(device: _selectU2())
    }

    public static func attachHostname(_ hostname: String) -> Flipper? {
        return hostname.withCString {
            let mutPtr = UnsafeMutablePointer(mutating: $0)
            guard let device = carbon_attach_hostname(mutPtr) else { return nil}
            return Flipper(device: device)
        }
    }

    /// FIXME: This should not be needed.
    private static func _selectU2() -> UnsafeMutablePointer<_lf_device> {
        let ctxPtr = lf_get_selected().pointee._dev_ctx.assumingMemoryBound(to: _carbon_context.self)
        lf_select(ctxPtr.pointee._u2)
        return lf_get_selected()
    }
}
