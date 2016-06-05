import com.sun.jna.Memory;
import com.sun.jna.Pointer;

/**
 * Created by Nick Mosher on 5/31/16.
 */
public class Usart {

    static final int USART0 = 0;
    static final int USART1 = 1;
    static final int DBGU = 2;

    private Flipper mFlipper;
    private int mPort;

    Usart(Flipper flipper, int port) {
        mFlipper = flipper;
        mPort = port;
        if(mPort < 0 || mPort > 2) throw new IllegalArgumentException("Usart port out of bounds.");
    }

    /**
     * Enables this USART bus.
     */
    public void enable() {
        mFlipper.error.clear();
        switch(mPort) {
            case USART0:
                mFlipper.getBinding().usart0_enable();
                break;
            case USART1:
                mFlipper.getBinding().usart1_enable();
                break;
            case DBGU:
                mFlipper.getBinding().dbgu_enable();
                break;
            default:
                throw new IllegalStateException("Usart port " + mPort + " is invalid.");
        }
    }

    /**
     * Disables this USART bus.
     */
    public void disable() {
        mFlipper.error.clear();
        switch(mPort) {
            case USART0:
                mFlipper.getBinding().usart0_disable();
                break;
            case USART1:
                mFlipper.getBinding().usart1_disable();
                break;
            case DBGU:
                mFlipper.getBinding().dbgu_disable();
                break;
            default:
                throw new IllegalStateException("Usart port " + mPort + " is invalid.");
        }
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Determines whether this USART bus is ready to send or receive data.
     * @return True if this bus is ready, false otherwise.
     */
    public boolean ready() {
        mFlipper.error.clear();
        byte ready;
        switch(mPort) {
            case USART0:
                ready = mFlipper.getBinding().usart0_ready();
                break;
            case USART1:
                ready = mFlipper.getBinding().usart1_ready();
                break;
            case DBGU:
                ready = mFlipper.getBinding().dbgu_ready();
                break;
            default:
                throw new IllegalStateException("Usart port " + mPort + " is invalid.");
        }
        Error.checkErrorCode(mFlipper.error.get());
        return ready != 0;
    }

    /**
     * Puts (transmits) a byte of data over this USART bus.
     * @param data The byte to send.
     */
    public void put(byte data) {
        mFlipper.error.clear();
        switch(mPort) {
            case USART0:
                mFlipper.getBinding().usart0_put(data);
                break;
            case USART1:
                mFlipper.getBinding().usart1_put(data);
                break;
            case DBGU:
                mFlipper.getBinding().dbgu_put(data);
                break;
            default:
                throw new IllegalStateException("Usart port " + mPort + " is invalid.");
        }
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Gets a byte of data from this USART bus.
     * @return A byte received from this USART bus.
     */
    public byte get() {
        mFlipper.error.clear();
        byte data;
        switch(mPort) {
            case USART0:
                data = mFlipper.getBinding().usart0_get();
                break;
            case USART1:
                data = mFlipper.getBinding().usart1_get();
                break;
            case DBGU:
                data = mFlipper.getBinding().dbgu_get();
                break;
            default:
                throw new IllegalStateException("Usart port " + mPort + " is invalid.");
        }
        Error.checkErrorCode(mFlipper.error.get());
        return data;
    }

    /**
     * Pushes all data from the given byte array over this USART bus.
     * @param source The byte array with data to be pushed.
     */
    public void push(byte[] source) {
        push(source, source.length);
    }

    /**
     * Pushes data from the given byte array over this USART bus.
     * @param source The byte array with data to be pushed.
     * @param length The length of data (in bytes) to push.
     */
    public void push(byte[] source, int length) {
        Pointer pointer = new Memory(length);
        pointer.write(0, source, 0, length);
        push(pointer, length);
    }

    /**
     * Pushes data from the given JNA pointer over this USART bus.
     * @param source The JNA pointer with data to be pushed.
     * @param length The length of data (in bytes) to push.
     */
    public void push(Pointer source, int length) {
        mFlipper.error.clear();
        switch(mPort) {
            case USART0:
                mFlipper.getBinding().usart0_push(source, length);
                break;
            case USART1:
                mFlipper.getBinding().usart1_push(source, length);
                break;
            case DBGU:
                mFlipper.getBinding().dbgu_push(source, length);
                break;
            default:
                throw new IllegalStateException("Usart port " + mPort + " is invalid.");
        }
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Pushes data from the given Structure over this USART bus.
     * @param source The Structure with data to be pushed.
     */
    public void push(Structure source) {
        mFlipper.error.clear();
        switch(mPort) {
            case USART0:
                mFlipper.getBinding().usart0_push(source, source.calculateSize());
                break;
            case USART1:
                mFlipper.getBinding().usart1_push(source, source.calculateSize());
                break;
            case DBGU:
                mFlipper.getBinding().dbgu_push(source, source.calculateSize());
                break;
            default:
                throw new IllegalStateException("Usart port " + mPort + " is invalid.");
        }
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Pulls data from this USART bus and stores it into the given byte buffer
     * until the buffer is full. Be cautious, this will block until enough data
     * is pulled to completely fill the given buffer.
     * @param destination The byte array buffer that the data is pulled into.
     */
    public void pull(byte[] destination) {
        pull(destination, destination.length);
    }

    /**
     * Pulls data from this USART bus and stores it into the given byte buffer.
     * @param destination The byte array buffer that the data is pulled into.
     * @param length The length of data (in bytes) to pull.
     */
    public void pull(byte[] destination, int length) {
        Pointer pointer = new Memory(length);
        pull(pointer, length);
        pointer.read(0, destination, 0, length);
    }

    /**
     * Pulls data from this USART bus and stores it into the given JNA Pointer.
     * @param destination The JNA Pointer that data is pulled into.
     * @param length The length of data (in bytes) to pull.
     */
    public void pull(Pointer destination, int length) {
        mFlipper.error.clear();
        switch(mPort) {
            case USART0:
                mFlipper.getBinding().usart0_pull(destination, length);
                break;
            case USART1:
                mFlipper.getBinding().usart1_pull(destination, length);
                break;
            case DBGU:
                mFlipper.getBinding().dbgu_pull(destination, length);
                break;
            default:
                throw new IllegalStateException("Usart port " + mPort + " is invalid.");
        }
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Pulls data from this USART bus and stores it into the given Structure.
     * @param destination The Structure that data is pulled into.
     */
    public void pull(Structure destination) {
        mFlipper.error.clear();
        switch(mPort) {
            case USART0:
                mFlipper.getBinding().usart0_pull(destination, destination.calculateSize());
                break;
            case USART1:
                mFlipper.getBinding().usart0_pull(destination, destination.calculateSize());
                break;
            case DBGU:
                mFlipper.getBinding().usart0_pull(destination, destination.calculateSize());
                break;
            default:
                throw new IllegalStateException("Usart port " + mPort + " is invalid.");
        }
        Error.checkErrorCode(mFlipper.error.get());
    }
}