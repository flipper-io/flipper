package io.flipper.java.drivers;

/**
 * Created by Nick Mosher on 4/1/16.
 */
public class Io {

    private Flipper mFlipper;

    Io(Flipper flipper) {
        mFlipper = flipper;
    }

    public void configure() {
        mFlipper.error.clear();
        mFlipper.getBinding().io_configure();
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Sets the direction of this pin. True means output, false means input.
     * @param direction The direction (input or output) for this pin.
     */
    public void direction(int pin, boolean direction) {
        mFlipper.error.clear();
        mFlipper.getBinding().io_set_direction((byte) pin, direction ? (byte) 1 : (byte) 0);
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Write a boolean value to this IO output.
     * @param value The boolean value to write.
     */
    public void write(int pin, boolean value) {
        mFlipper.error.clear();
        if(pin < 0 || pin > 16) {
            throw new IllegalArgumentException("Cannot write boolean value to analog pin.");
        }

        mFlipper.getBinding().io_write((byte) pin, value ? (byte) 1 : (byte) 0);
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Write an analog value to this IO output.
     * @param value The analog value to write.
     */
    public void write(int pin, short value) {
        mFlipper.error.clear();
        if(pin < 17) {
            throw new IllegalArgumentException("Cannot write analog value to digital pin.");
        }

        mFlipper.getBinding().io_write((byte) pin, value);
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Reads a raw value from the given pin. Digital values are distinguished by zero or
     * non-zero, analog values range within the whole range of a short (16-bit) value.
     * @param pin The pin to read from .
     * @return The raw value of the given pin.
     */
    public short read(int pin) {
        mFlipper.error.clear();
        short value = mFlipper.getBinding().io_read((byte) pin);
        Error.checkErrorCode(mFlipper.error.get());
        return value;
    }

    /**
     * Reads a digital value from the given pin and performs a sanity check that the given
     * pin is actually a digital pin on the device.
     * @param pin The pin to read a digital value from.
     * @return True for pin HIGH, false for pin LOW.
     */
    public boolean digitalRead(int pin) {
        mFlipper.error.clear();
        if(pin < 0 || pin > 16) {
            throw new IllegalArgumentException("Cannot read boolean value from analog pin.");
        }

        boolean value = mFlipper.getBinding().io_read((byte) pin) != 0;
        Error.checkErrorCode(mFlipper.error.get());
        return value;
    }

    /**
     * Reads an analog value from the given pin and performs a sanity check that the given
     * pin is actually an analog pin on the device.
     * @param pin The pin to read an analog value from.
     * @return A 16-bit short value indicating the analog value of the given pin.
     */
    public short analogRead(int pin) {
        mFlipper.error.clear();
        if(pin < 17) {
            throw new IllegalArgumentException("Cannot read analog value from digital pin.");
        }

        short value = mFlipper.getBinding().io_read((byte) pin);
        Error.checkErrorCode(mFlipper.error.get());
        return value;
    }
}
