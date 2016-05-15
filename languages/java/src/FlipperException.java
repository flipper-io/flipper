/**
 * Created by Nick Mosher on 5/14/16.
 */
public class FlipperException extends RuntimeException {

    public FlipperException() { }
    public FlipperException(String message) { super(message); }

    public static class FMRChecksumWrongException extends FlipperException {
        public FMRChecksumWrongException() { }
        public FMRChecksumWrongException(String message) { super(message); }
    }

    public static class MemoryAllocationFailedException extends FlipperException {
        public MemoryAllocationFailedException() { }
        public MemoryAllocationFailedException(String message) { super(message); }
    }

    public static class TooManyArgumentsException extends FlipperException {
        public TooManyArgumentsException() { }
        public TooManyArgumentsException(String message) { super(message); }
    }

    public static class FVMLoadFailedException extends FlipperException {
        public FVMLoadFailedException() { }
        public FVMLoadFailedException(String message) { super(message); }
    }

    public static class FVMSymbolFailedException extends FlipperException {
        public FVMSymbolFailedException() { }
        public FVMSymbolFailedException(String message) { super(message); }
    }

    public static class SocketOpenFailedException extends FlipperException {
        public SocketOpenFailedException() { }
        public SocketOpenFailedException(String message) { super(message); }
    }

    public static class SocketConnectFailedException extends FlipperException {
        public SocketConnectFailedException() { }
        public SocketConnectFailedException(String message) { super(message); }
    }

    public static class FlipperUnboundException extends FlipperException {
        public FlipperUnboundException() { }
        public FlipperUnboundException(String message) { super(message); }
    }

    public static class FlipperNotFoundException extends FlipperException {
        public FlipperNotFoundException() { }
        public FlipperNotFoundException(String message) { super(message); }
    }

    public static class HIDManagerFailedException extends FlipperException {
        public HIDManagerFailedException() { }
        public HIDManagerFailedException(String message) { super(message); }
    }

    public static class HIDManagerNoDeviceException extends FlipperException {
        public HIDManagerNoDeviceException() { }
        public HIDManagerNoDeviceException(String message) { super(message); }
    }

    public static class HIDTooManyDevicesException extends FlipperException {
        public HIDTooManyDevicesException() { }
        public HIDTooManyDevicesException(String message) { super(message); }
    }

    public static class HIDOpenDeviceFailedException extends FlipperException {
        public HIDOpenDeviceFailedException() { }
        public HIDOpenDeviceFailedException(String message) { super(message); }
    }

    public static class HIDDeviceDisconnectedException extends FlipperException {
        public HIDDeviceDisconnectedException() { }
        public HIDDeviceDisconnectedException(String message) { super(message); }
    }

    public static class HIDWriteFailedException extends FlipperException {
        public HIDWriteFailedException() { }
        public HIDWriteFailedException(String message) { super(message); }
    }

    public static class HIDTimeoutException extends FlipperException {
        public HIDTimeoutException() { }
        public HIDTimeoutException(String message) { super(message); }
    }

    public static class IOKitDictionaryException extends FlipperException {
        public IOKitDictionaryException() { }
        public IOKitDictionaryException(String message) { super(message); }
    }

    public static class DynamicLibraryNotFoundException extends FlipperException {
        public DynamicLibraryNotFoundException() { }
        public DynamicLibraryNotFoundException(String message) { super(message); }
    }

    public static class DynamicLibraryLoadFailureException extends FlipperException {
        public DynamicLibraryLoadFailureException() { }
        public DynamicLibraryLoadFailureException(String message) { super(message); }
    }

    public static class DynamicLibraryAlreadyLoadedException extends FlipperException {
        public DynamicLibraryAlreadyLoadedException() { }
        public DynamicLibraryAlreadyLoadedException(String message) { super(message); }
    }

    public static class FileOpenFailureException extends FlipperException {
        public FileOpenFailureException() { }
        public FileOpenFailureException(String message) { super(message); }
    }

    public static class AddFileFailureException extends FlipperException {
        public AddFileFailureException() { }
        public AddFileFailureException(String message) { super(message); }
    }

    public static class NoFileFailureException extends FlipperException {
        public NoFileFailureException() { }
        public NoFileFailureException(String message) { super(message); }
    }

    public static class UnimplementedException extends FlipperException {
        public UnimplementedException() { }
        public UnimplementedException(String message) { super(message); }
    }

    public static class UnknownErrorException extends FlipperException {
        public UnknownErrorException() { }
        public UnknownErrorException(String message) { super(message); }
    }
}
