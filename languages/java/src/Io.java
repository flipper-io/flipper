/**
 * Created by Nick Mosher on 4/1/16.
 */
public class Io {

    private Flipper._flipper mFlipper;

    public Io(Flipper._flipper flipper) {
        mFlipper = flipper;
    }

    public void configure() {
        mFlipper.io_configure();
    }

    /**
     * Sets the direction of this pin. True means output, false means input.
     * @param direction The direction (input or output) for this pin.
     */
    public void direction(int pin, boolean direction) {
        mFlipper.io_set_direction((byte) pin, direction ? (byte) 1 : (byte) 0);
    }

    /**
     * Write a boolean value to this IO output.
     * @param value The boolean value to write.
     */
    public void write(int pin, boolean value) {
        if(pin < 0 || pin > 16) {
            throw new IllegalArgumentException("Cannot write boolean value to analog pin.");
        }

        mFlipper.io_write((byte) pin, value ? (byte) 1 : (byte) 0);
    }

    /**
     * Write an analog value to this IO output.
     * @param value The analog value to write.
     */
    public void write(int pin, short value) {
        if(pin < 17) {
            throw new IllegalArgumentException("Cannot write analog value to digital pin.");
        }

        mFlipper.io_write((byte) pin, value);
    }

    public boolean digitalRead(int pin) {
        if(pin < 0 || pin > 16) {
            throw new IllegalArgumentException("Cannot read boolean value from analog pin.");
        }

        return mFlipper.io_read((byte) pin) != 0;
    }

    public short analogRead(int pin) {
        if(pin < 17) {
            throw new IllegalArgumentException("Cannot read analog value from digital pin.");
        }

        return mFlipper.io_read((byte) pin);
    }
}
