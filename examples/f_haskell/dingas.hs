import Flipper.Internal.Error
import Flipper.Internal.Flipper

main = disclose >> attach USB "derp" >>= print
