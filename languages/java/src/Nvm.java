import com.sun.jna.Memory;
import com.sun.jna.Pointer;

/**
 * Created by Nick Mosher on 5/31/16.
 */
public class Nvm {

    private Flipper mFlipper;

    Nvm(Flipper flipper) {
        mFlipper = flipper;
    }

    /**
     * Configures Flipper's NVM driver.
     */
    public void configure() {
        mFlipper.error.clear();
        mFlipper.getBinding().nvm_configure();
        Error.checkErrorCode(mFlipper.error.get());
    }

    public void enable() {
        mFlipper.error.clear();
        mFlipper.getBinding().nvm_enable();
        Error.checkErrorCode(mFlipper.error.get());
    }

    public void disable() {
        mFlipper.error.clear();
        mFlipper.getBinding().nvm_disable();
        Error.checkErrorCode(mFlipper.error.get());
    }

    public void reset() {
        mFlipper.error.clear();
        mFlipper.getBinding().nvm_reset();
        Error.checkErrorCode(mFlipper.error.get());
    }

    public void read(Fs.FilePointer pointer) {
        mFlipper.error.clear();
        mFlipper.getBinding().nvm_read(pointer.getRawAddress());
        Error.checkErrorCode(mFlipper.error.get());
    }

    public byte get() {
        mFlipper.error.clear();
        byte value = mFlipper.getBinding().nvm_get();
        Error.checkErrorCode(mFlipper.error.get());
        return value;
    }

    /**
     * Allocates a segment of flash memory of `length` bytes, returning a FilePointer
     * with the address of the newly allocated space.
     * @param length The length of data to allocate (in bytes).
     * @return A FilePointer to the address of the allocated space in Flipper's filesystem.
     */
    public Fs.FilePointer alloc(int length) {
        mFlipper.error.clear();
        int address = mFlipper.getBinding().nvm_alloc(length);
        Error.checkErrorCode(mFlipper.error.get());
        return new Fs.FilePointer(address);
    }

    /**
     * Frees a previously allocated segment of flash memory. Use this when the
     * file is no longer needed, as it may be deleted and cannot be retrieved
     * after freeing.
     * @param pointer The FilePointer to the file being freed from Flipper's flash memory.
     */
    public void free(Fs.FilePointer pointer) {
        mFlipper.error.clear();
        mFlipper.getBinding().nvm_free(pointer.getRawAddress());
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Formats Flipper's flash memory.
     */
    public void format() {
        mFlipper.error.clear();
        mFlipper.getBinding().nvm_format();
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Push data to Flipper's flash memory and store it into `destination`.
     * @param data A byte array of data to push to Flipper's filesystem.
     * @param destination The FilePointer to the file on Flipper's filesystem to push data to.
     */
    public void push(byte[] data, Fs.FilePointer destination) {
        push(data, data.length, destination);
    }

    /**
     * Push data to Flipper's flash memory and store it into `destination`.
     * @param data A byte array of data to push to Flipper's filesystem.
     * @param length The length of data to push (in bytes).
     * @param destination The FilePointer to the file on Flipper's filesystem to push data to.
     */
    public void push(byte[] data, int length, Fs.FilePointer destination) {
        if(length > data.length) {
            throw new ArrayIndexOutOfBoundsException("Given length to write is larger than buffer.");
        }

        //Allocate explicit memory for the data and get a pointer to it.
        Pointer pointer = new Memory(length);
        pointer.write(0, data, 0 ,length);

        //Call overloaded push with Pointer.
        push(pointer, length, destination);
    }

    /**
     * Push data to Flipper's flash memory and store it into `destination`.
     * @param data The JNA Pointer object to store the retrieved data in.
     *                This must be initialized as `Pointer pointer = new Memory(length);`,
     *                where `length` is greater than or equal to the `length` passed below.
     * @param length The length of data to pull (in bytes).
     * @param destination The FilePointer to the file on Flipper's filesystem to push data to.
     */
    public void push(Pointer data, int length, Fs.FilePointer destination) {
        mFlipper.error.clear();
        mFlipper.getBinding().nvm_push(data, length, destination.getRawAddress());
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Pull data from Flipper's flash memory and store it into `buffer`.
     * @param buffer A byte array to store the retrieved data in.
     * @param source The FilePointer to the file on Flipper's filesystem to pull data from.
     */
    public void pull(byte[] buffer, Fs.FilePointer source) {
        pull(buffer, buffer.length, source);
    }

    /**
     * Pull data from Flipper's flash memory and store it into `buffer`.
     * @param buffer A byte array to store the retrieved data in.
     * @param length The length of data to pull (in bytes).
     * @param source The FilePointer to the file on Flipper's filesystem to pull data from.
     */
    public void pull(byte[] buffer, int length, Fs.FilePointer source) {
        if(length > buffer.length) {
            throw new ArrayIndexOutOfBoundsException("Given length to read is larger than buffer.");
        }

        //Allocate explicit memory for the data to be received.
        Pointer pointer = new Memory(length);

        //Call overloaded pull with Pointer.
        pull(pointer, length, source);

        //Copy data from C-style pointer into Java byte[] buffer.
        pointer.read(0, buffer, 0, length);
    }

    /**
     * Pull data from Flipper's flash memory and store it into `buffer`.
     * @param buffer The JNA Pointer object to store the retrieved data in.
     *                This must be initialized as `Pointer buffer = new Memory(length);`,
     *                where `length` is greater than or equal to the length parameter.
     * @param length The length of data to pull (in bytes).
     * @param source The FilePointer to the file on Flipper's filesystem to pull data from.
     */
    public void pull(Pointer buffer, int length, Fs.FilePointer source) {
        mFlipper.error.clear();
        mFlipper.getBinding().nvm_pull(buffer, length, source.getRawAddress());
        Error.checkErrorCode(mFlipper.error.get());
    }
}
