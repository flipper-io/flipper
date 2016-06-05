import com.sun.jna.Memory;
import com.sun.jna.Pointer;

/**
 * Created by Nick Mosher on 6/2/16.
 */
public class Spi {

    private Flipper mFlipper;

    Spi(Flipper flipper) {
        mFlipper = flipper;
    }

    /**
     * Configures Flipper's SPI driver.
     */
    public void configure() {
        mFlipper.error.clear();
        mFlipper.getBinding().spi_configure();
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Enables Flipper's SPI driver.
     */
    public void enable() {
        mFlipper.error.clear();
        mFlipper.getBinding().spi_enable();
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Disables Flipper's SPI driver.
     */
    public void disable() {
        mFlipper.error.clear();
        mFlipper.getBinding().spi_disable();
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Checks whether Flipper's SPI driver is ready.
     * @return True if ready, false otherwise.
     */
    public boolean ready() {
        mFlipper.error.clear();
        byte value = mFlipper.getBinding().spi_ready();
        Error.checkErrorCode(mFlipper.error.get());
        return value != 0;
    }

    /**
     * Sends one byte of data over Flipper's SPI bus.
     * @param data The byte of data to send.
     */
    public void put(byte data) {
        mFlipper.error.clear();
        mFlipper.getBinding().spi_put(data);
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Receives one byte of data incoming from Flipper's SPI bus.
     * @return The next byte of data received.
     */
    public byte get() {
        mFlipper.error.clear();
        byte value = mFlipper.getBinding().spi_get();
        Error.checkErrorCode(mFlipper.error.get());
        return value;
    }

    /**
     * Sends an array of bytes over Flipper's SPI bus.
     * @param source The array of bytes to send.
     */
    public void push(byte[] source) {
        push(source, source.length);
    }

    /**
     * Sends a certain number of bytes from an array over Flipper's SPI bus.
     * @param source The byte array to send data from.
     * @param length The number of bytes to send. Must be less than or equal
     *               to the length of the given data array.
     */
    public void push(byte[] source, int length) {
        if(length > source.length) {
            throw new ArrayIndexOutOfBoundsException("Given length to push is larger than buffer.");
        }

        //Allocate explicit memory for the data being sent.
        Pointer pointer = new Memory(length);

        //Copy the data to push into the pointer.
        pointer.write(0, source, 0, length);

        //Call overloaded push with Pointer.
        push(pointer, length);
    }

    /**
     * Sends data from a JNA Pointer object over Flipper's SPI bus.
     * @param source The JNA Pointer containing data to send.
     * @param length The length of data (in bytes) to send from the Pointer.
     *               This must be less than or equal to the length of memory
     *               allocated for the given Pointer.
     */
    public void push(Pointer source, int length) {
        mFlipper.error.clear();
        mFlipper.getBinding().spi_push(source, length);
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Sends data from a Structure object over Flipper's SPI bus.
     * @param source The Structure containing data to send.
     */
    public void push(Structure source) {
        mFlipper.error.clear();
        mFlipper.getBinding().spi_push(source, source.calculateSize());
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Receives incoming data from Flipper's SPI bus and stores it into a
     * byte array. Caution: This will block until enough data is received
     * to completely fill the given array.
     * @param destination The buffer to store received data in.
     */
    public void pull(byte[] destination) {
        pull(destination, destination.length);
    }

    /**
     * Receives incoming data from Flipper's SPI bus and stores it into a
     * byte array. Caution: This will block until the specified amount of
     * data (in bytes) is received.
     * @param destination The buffer to store received data in.
     * @param length The number of bytes to receive. This must be less than
     *               or equal to the length of the given buffer.
     */
    public void pull(byte[] destination, int length) {
        if(length > destination.length) {
            throw new ArrayIndexOutOfBoundsException("Given length to pull is larger than destination buffer.");
        }

        //Allocate explicit memory for the data being retrieved.
        Pointer pointer = new Memory(length);

        //Call overloaded pull with Pointer.
        pull(pointer, length);

        //Copy the retrieved data from the pointer into the buffer.
        pointer.read(0, destination, 0, length);
    }

    /**
     * Receives incoming data from Flipper's SPI bus and stores it into a
     * JNA Pointer object. Caution: This will block until the specified amount
     * of data (in bytes) is received.
     * @param destination The JNA Pointer to store received data in.
     * @param length The number of bytes to receive. This must be less than or
     *               equal to the length of memory allocated for the given Pointer.
     */
    public void pull(Pointer destination, int length) {
        mFlipper.error.clear();
        mFlipper.getBinding().spi_pull(destination, length);
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Receives incoming data from Flipper's SPI bus and stores it into a
     * Structure object.
     * @param destination The Structure subclass to store received data in.
     */
    public void pull(Structure destination) {
        mFlipper.error.clear();
        mFlipper.getBinding().spi_pull(destination, destination.calculateSize());
        Error.checkErrorCode(mFlipper.error.get());
    }
}