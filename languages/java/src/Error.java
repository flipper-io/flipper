/**
 * Created by nick on 5/14/16.
 */
class Error {

    private Flipper mFlipper;

    Error(Flipper flipper) {
        mFlipper = flipper;
    }

    public void configure() {
        mFlipper.getBinding().error_configure();
    }

    public void withold() {
        mFlipper.getBinding().error_withold();
    }

    public void disclose() {
        mFlipper.getBinding().error_disclose();
    }

    public void raise(short code, String message) {
        mFlipper.getBinding().error_raise(code, message);
    }

    public short get() {
        return mFlipper.getBinding().error_get();
    }

    public void clear() {
        mFlipper.getBinding().error_clear();
    }

    public static void checkErrorCode(short code) {
        switch(code) {
            case 0:
                return;
            case 1:
                throw new FlipperException.FMRChecksumWrongException();
            case 2:
                throw new FlipperException.MemoryAllocationFailedException();
            case 3:
                throw new FlipperException.TooManyArgumentsException();
            case 4:
                throw new FlipperException.FVMLoadFailedException();
            case 5:
                throw new FlipperException.FVMSymbolFailedException();
            case 6:
                throw new FlipperException.SocketOpenFailedException();
            case 7:
                throw new FlipperException.SocketConnectFailedException();
            case 8:
                throw new FlipperException.FlipperUnboundException();
            case 9:
                throw new FlipperException.FlipperNotFoundException();
            case 10:
                throw new FlipperException.HIDManagerFailedException();
            case 11:
                throw new FlipperException.HIDManagerNoDeviceException();
            case 12:
                throw new FlipperException.HIDTooManyDevicesException();
            case 13:
                throw new FlipperException.HIDOpenDeviceFailedException();
            case 14:
                throw new FlipperException.HIDDeviceDisconnectedException();
            case 15:
                throw new FlipperException.HIDWriteFailedException();
            case 16:
                throw new FlipperException.HIDTimeoutException();
            case 17:
                throw new FlipperException.IOKitDictionaryException();
            case 18:
                throw new FlipperException.DynamicLibraryNotFoundException();
            case 19:
                throw new FlipperException.DynamicLibraryLoadFailureException();
            case 20:
                throw new FlipperException.DynamicLibraryAlreadyLoadedException();
            case 21:
                throw new FlipperException.FileOpenFailureException();
            case 22:
                throw new FlipperException.AddFileFailureException();
            case 23:
                throw new FlipperException.NoFileFailureException();
            case 24:
                throw new FlipperException.UnimplementedException();
            default:
                throw new FlipperException.UnknownErrorException();
        }
    }
}
