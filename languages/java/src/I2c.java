import com.sun.jna.Memory;
import com.sun.jna.Pointer;

/**
 * Created by Nick Mosher on 5/31/16.
 */
public class I2c {

    private Flipper mFlipper;

    I2c(Flipper flipper) {
        mFlipper = flipper;
    }

    /**
     * Configures the I2C driver.
     */
    public void configure() {
        mFlipper.error.clear();
        mFlipper.getBinding().i2c_configure();
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Writes a byte array of data to an I2C device attached to Flipper.
     * @param mode The I2C mode.
     * @param address The I2C device address (i.e. which device to transmit to).
     * @param source An array containing data to write to the I2C device.
     * @return TODO what does this return?
     */
    public int put(int mode, int address, byte[] source) {
        return put(mode, address, source, source.length);
    }

    /**
     * Writes a byte array of data to an I2C device attached to Flipper.
     * @param mode The I2C mode.
     * @param address The I2C device address (i.e. which device to transmit to).
     * @param source An array containing data to write to the I2C device.
     * @param length The number of bytes from `data` to write.
     * @return TODO what does this return?
     */
    public int put(int mode, int address, byte[] source, int length) {
        mFlipper.error.clear();

        if(length > source.length) {
            throw new ArrayIndexOutOfBoundsException("Given length to write is larger than buffer.");
        }

        //Allocate explicit memory for the data and get a pointer to it.
        Pointer pointer = new Memory(length);
        pointer.write(0, source, 0, length);

        //Call overloaded put with Pointer.
        return put(mode, address, pointer, length);
    }

    /**
     * Writes data to an I2C device attached to Flipper from a JNA Pointer object.
     * @param mode The I2C mode.
     * @param address The I2C device address (i.e. which device to send to).
     * @param source A JNA Pointer object containing data to send.
     *               This must be initialized as `Pointer buffer = new Memory(length);`,
     *               where `length` is greater than or equal to the length parameter.
     * @param length The length of data to put (in bytes).
     * @return TODO what does this return?
     */
    public int put(int mode, int address, Pointer source, int length) {
        mFlipper.error.clear();
        int value = mFlipper.getBinding().i2c_put(mode, address, source, length);
        Error.checkErrorCode(mFlipper.error.get());
        return value;
    }

    /**
     * Writes data to an I2C device attached to Flipper from a JNA Structure.
     * @param mode The I2C mode.
     * @param address The I2C device address (i.e. which device to send to).
     * @param source A Structure containing data to send.
     * @return TODO what does this return?
     */
    public int put(int mode, int address, Structure source) {
        mFlipper.error.clear();
        int value = mFlipper.getBinding().i2c_put(mode, address, source, source.calculateSize());
        Error.checkErrorCode(mFlipper.error.get());
        return value;
    }

    /**
     * Reads data from an I2C device attached to Flipper into a byte array buffer.
     * @param mode The I2C mode.
     * @param address The I2C device address (i.e. which device to receive from).
     * @param destination A byte array in which to store the received data.
     * @return TODO what does this return?
     */
    public int get(int mode, int address, byte[] destination) {
        return get(mode, address, destination, destination.length);
    }

    /**
     * Reads data from an I2C device attached to Flipper into a byte array buffer.
     * @param mode The I2C mode.
     * @param address The I2C device address (i.e. which device to receive from).
     * @param destination A byte array in which to store the received data.
     * @param length The length of memory to read (i.e. how many bytes to read into the buffer).
     * @return TODO what does this return?
     */
    public int get(int mode, int address, byte[] destination, int length) {
        mFlipper.error.clear();

        if(length > destination.length) {
            throw new ArrayIndexOutOfBoundsException("Given length to read is larger than buffer.");
        }

        //Allocate explicit memory for the data to be received.
        Pointer pointer = new Memory(length);

        //Call overloaded get with Pointer.
        int value = get(mode, address, pointer, length);

        //Copy data from C-style pointer into Java byte[] buffer.
        pointer.read(0, destination, 0, length);
        return value;
    }

    /**
     * Reads data from an I2C device attached to Flipper into a JNA Pointer object.
     * @param mode The I2C mode.
     * @param address The I2C device address (i.e. which device to receive from).
     * @param destination A JNA Pointer object to store the retrieved data in.
     *               This must be initialized as `Pointer buffer = new Memory(length);`,
     *               where `length` is greater than or equal to the length parameter.
     * @param length The length of data to get (in bytes).
     * @return TODO what does this return?
     */
    public int get(int mode, int address, Pointer destination, int length) {
        mFlipper.error.clear();
        int value = mFlipper.getBinding().i2c_get(mode, address, destination, length);
        Error.checkErrorCode(mFlipper.error.get());
        return value;
    }

    /**
     * Reads data from an I2C device attached to Flipper into a JNA Pointer object.
     * @param mode The I2C mode.
     * @param address The I2C device address (i.e. which device to receive from).
     * @param destination A child class of Structure to store received data.
     * @return TODO what does this return?
     */
    public int get(int mode, int address, Structure destination) {
        mFlipper.error.clear();
        int value = mFlipper.getBinding().i2c_get(mode, address, destination, destination.calculateSize());
        Error.checkErrorCode(mFlipper.error.get());
        return value;
    }
}