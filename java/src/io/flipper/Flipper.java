/*
 * JNA is not a product of Flipper.  It's home can be found
 * at https://github.com/java-native-access/jna.  Below is
 * a copy of it's license.
 * --------------------------------------------------------
 *
 * JNA is dual-licensed under 2 alternative Open Source/Free
 * licenses: LGPL 2.1 and Apache License 2.0. (starting with
 * JNA version 4.0.0).
 *
 * What this means is that one can choose either one of these
 * licenses (for purposes of re-distributing JNA; usually by
 * including it as one of jars another application or
 * library uses) by downloading corresponding jar file,
 * using it, and living happily everafter.
 *
 * You may obtain a copy of the LGPL License at:
 *
 * http://www.gnu.org/licenses/licenses.html
 *
 * A copy is also included in the downloadable source code package
 * containing JNA, in file "LGPL2.1", under the same directory
 * as this file.
 *
 * You may obtain a copy of the ASL License at:
 *
 * http://www.apache.org/licenses/
 *
 * A copy is also included in the downloadable source code package
 * containing JNA, in file "ASL2.0", under the same directory
 * as this file.
 */

package io.flipper;

import com.sun.jna.Library;
import com.sun.jna.Native;

/**
 * Created by Nick Mosher on 10/3/15.
 * Flipper's Java Wrapper library.
 */
public class Flipper {

    public static final char SOURCE_USB = (char) 0;
    public static final char SOURCE_NETWORK = (char) 1;

    public final PWM pwm0 = PWM.pwm0;
    public final PWM pwm1 = PWM.pwm1;
    public final PWM pwm2 = PWM.pwm2;
    public final PWM pwm3 = PWM.pwm3;
    public final IO io = IO.io;

    private static _flipper mFlipper;

    static {
        mFlipper = (_flipper) Native.loadLibrary("flipper", _flipper.class);
    }

    /**
     * JNA uses interface definitions to load native libraries by
     * identifying native methods by the same name as the interface
     * methods and linking them.  JNA "inflates" an instance of
     * this interface which allows us to call the methods.
     */
    private interface _flipper extends Library {
        void flipper_attach(char source);
        void led_set_rgb(int r, int g, int b);
        boolean button_read();

        /* ~ IO Bindings ~ */
        void io_configure();
        void io_set_direction(byte pin, byte direction);
        void io_write(byte pin, short value);
        short io_read(byte pin);

        /* ~ PWM Bindings ~ */
        void pwm0_configure();
        void pwm0_enable();
        void pwm0_disable();
        void pwm0_enable_interrupt();
        void pwm0_disable_interrupt();
        boolean pwm0_enabled();
        boolean pwm0_interrupt_enabled();

        boolean pwm0_finished_cycle();
        void pwm1_configure();
        void pwm1_enable();
        void pwm1_disable();
        void pwm1_enable_interrupt();
        void pwm1_disable_interrupt();
        boolean pwm1_enabled();
        boolean pwm1_interrupt_enabled();

        boolean pwm1_finished_cycle();
        void pwm2_configure();
        void pwm2_enable();
        void pwm2_disable();
        void pwm2_enable_interrupt();
        void pwm2_disable_interrupt();
        boolean pwm2_enabled();
        boolean pwm2_interrupt_enabled();

        boolean pwm2_finished_cycle();
        void pwm3_configure();
        void pwm3_enable();
        void pwm3_disable();
        void pwm3_enable_interrupt();
        void pwm3_disable_interrupt();
        boolean pwm3_enabled();
        boolean pwm3_interrupt_enabled();
        boolean pwm3_finished_cycle();
    }

    /*
     * In the future when we add support for multiple Flippers to be
     * connected at once, we'll switch to an object-based approach
     * instead of a static one.
     */
    public Flipper() {

    }

    /**
     * IO API for Flipper.
     */
    public enum IO {

        //Declare IO instances.
        io;

        public static final boolean LOW = false;
        public static final boolean HIGH = true;

        private static final short LOW_BYTE = (short) 0;
        private static final short HIGH_BYTE = (short) 1;

        /**
         * Enumeration to help selecting IO pins.
         */
        public enum Pin {
            IO1((byte) 1),
            IO2((byte) 2),
            IO3((byte) 3),
            IO4((byte) 4),
            IO5((byte) 5),
            IO6((byte) 6),
            IO7((byte) 7),
            IO8((byte) 8),
            IO9((byte) 9),
            IO10((byte) 10),
            IO11((byte) 11),
            IO12((byte) 12),
            IO13((byte) 13),
            IO14((byte) 14),
            IO15((byte) 15),
            IO16((byte) 16),

            A0((byte) 17),
            A1((byte) 18),
            A2((byte) 19),
            A3((byte) 20),
            A4((byte) 21),
            A5((byte) 22),
            A6((byte) 23),
            A7((byte) 24),
            A8((byte) 25);

            private byte mPin;

            Pin(byte pinNumber) {
                mPin = pinNumber;
            }

            private byte getPin() {
                return mPin;
            }
        }

        /**
         * Enumeration to help selecting IO direction.
         */
        public enum Direction {
            INPUT((byte) 0),
            OUTPUT((byte) 1);

            private byte mDirection;

            Direction(byte direction) {
                mDirection = direction;
            }

            private byte getDirection() {
                return mDirection;
            }
        }

        /**
         * Configures Flipper's IO bus.
         */
        public void configure() {
            mFlipper.io_configure();
        }

        /**
         * Sets the direction for the given IO pin.
         * @param pin The pin to configure.
         * @param direction The direction to set the pin (Direction.INPUT, Direction,OUTPUT).
         */
        public void setDirection(Pin pin, Direction direction) {
            mFlipper.io_set_direction(pin.getPin(), direction.getDirection());
        }

        /**
         * Writes a boolean value to the given pin.  The pin must be digital and be configured for OUTPUT mode.
         * @param pin The pin to write a digital value to.
         * @param value The value to write (IO.LOW, IO.HIGH).
         */
        public void write(Pin pin, boolean value) {

            //If this pin is in fact digital, write the boolean value.
            if(pin.getPin() <= 16) {
                mFlipper.io_write(pin.getPin(), value ? HIGH_BYTE : LOW_BYTE);

            //If this pin is not digital, we cannot write a boolean value to it.
            } else {
                throw new IllegalArgumentException("Cannot write a boolean value to an analog pin!");
            }
        }

        /**
         * Writes a boolean value to the given pin.  The pin must be digital and be configured for OUTPUT mode.
         * @param pin The pin to write a digital value to.
         * @param value The value to write (IO.LOW, IO.HIGH).
         */
        public void writeDigital(Pin pin, boolean value) {

            //If this pin is in fact digital, write the boolean value.
            if(pin.getPin() <= 16) {
                mFlipper.io_write(pin.getPin(), value ? HIGH_BYTE : LOW_BYTE);

            //If this pin is not digital, we cannot write a boolean value to it.
            } else {
                throw new IllegalArgumentException("Cannot write a boolean value to an analog pin!");
            }
        }

        /**
         * Writes a short (two-byte) value to the given pin.  The pin must be analog and be configured for OUTPUT mode.
         * @param pin The pin to write an analog value to.
         * @param value The value to write.
         */
        public void write(Pin pin, short value) {

            //If this pin is analog, write the short value.
            if(pin.getPin() >= 17) {
                mFlipper.io_write(pin.getPin(), value);

            //If this pin is not analog, we cannot write a short value to it.
            } else {
                throw new IllegalArgumentException("Cannot write a boolean value to an analog pin!");
            }
        }

        /**
         * Writes a short (two-byte) value to the given pin.  The pin must be analog and be configured for OUTPUT mode.
         * @param pin The pin to write an analog value to.
         * @param value The value to write.
         */
        public void writeAnalog(Pin pin, short value) {

            //If this pin is analog, write the short value.
            if(pin.getPin() >= 17) {
                mFlipper.io_write(pin.getPin(), value);

            //If this pin is not analog, we cannot write a short value to it.
            } else {
                throw new IllegalArgumentException("Cannot write a boolean value to an analog pin!");
            }
        }

        /**
         * Reads a digital value from the given pin.  The pin must be digital and be configured for INPUT mode.
         * @param pin The digital pin to read from.
         * @return The digital value at the pin.
         */
        public boolean readDigital(Pin pin) {

            //If the pin is digital, read the value.
            if(pin.getPin() <= 16) {
                return mFlipper.io_read(pin.getPin()) != 0;

            //If this pin is not digital, we cannot read a boolean value from it.
            } else {
                throw new IllegalArgumentException("Cannot read a digital value from an analog pin!");
            }
        }

        /**
         * Reads an analog value from the given pin.  The pin must be analog and configured for INPUT mode.
         * @param pin The analog pin to read from.
         * @return The analog value at the pin.
         */
        public short readAnalog(Pin pin) {

            //If this pin is analog, read the value.
            if(pin.getPin() >= 17) {
                return mFlipper.io_read(pin.getPin());

            //If this pin is not analog, we cannot read a short value from it.
            } else {
                throw new IllegalArgumentException("Cannot read an analog value from a digital pin!");
            }
        }
    }

    /**
     * PWM API for Flipper.
     */
    public enum PWM {

        //Declare PWM instances.
        pwm0(0),
        pwm1(1),
        pwm2(2),
        pwm3(3);

        private int mChannel;

        PWM(int channel) {
            mChannel = channel;
        }

        /**
         * Configures this PWM channel.
         */
        public void configure() {
            switch(mChannel) {
                case 0:
                    mFlipper.pwm0_configure();
                    break;
                case 1:
                    mFlipper.pwm1_configure();
                    break;
                case 2:
                    mFlipper.pwm2_configure();
                    break;
                case 3:
                    mFlipper.pwm3_configure();
                    break;
                default:
            }
        }

        /**
         * Enables this PWM channel.
         */
        public void enable() {
            switch(mChannel) {
                case 0:
                    mFlipper.pwm0_enable();
                    break;
                case 1:
                    mFlipper.pwm1_enable();
                    break;
                case 2:
                    mFlipper.pwm2_enable();
                    break;
                case 3:
                    mFlipper.pwm3_enable();
                    break;
                default:
            }
        }

        /**
         * Disables this PWM channel.
         */
        public void disable() {
            switch(mChannel) {
                case 0:
                    mFlipper.pwm0_disable();
                    break;
                case 1:
                    mFlipper.pwm1_disable();
                    break;
                case 2:
                    mFlipper.pwm2_disable();
                    break;
                case 3:
                    mFlipper.pwm3_disable();
                    break;
                default:
            }
        }

        /**
         * Enables interrupts on this PWM channel.
         */
        public void enableInterrupt() {
            switch(mChannel) {
                case 0:
                    mFlipper.pwm0_enable_interrupt();
                    break;
                case 1:
                    mFlipper.pwm1_enable_interrupt();
                    break;
                case 2:
                    mFlipper.pwm2_enable_interrupt();
                    break;
                case 3:
                    mFlipper.pwm3_enable_interrupt();
                    break;
                default:
            }
        }

        /**
         * Disables interrupts on this PWM channel.
         */
        public void disableInterrupt() {
            switch(mChannel) {
                case 0:
                    mFlipper.pwm0_disable_interrupt();
                    break;
                case 1:
                    mFlipper.pwm1_disable_interrupt();
                    break;
                case 2:
                    mFlipper.pwm2_disable_interrupt();
                    break;
                case 3:
                    mFlipper.pwm3_disable_interrupt();
                    break;
                default:
            }
        }

        /**
         * Returns whether this PWM channel is enabled.
         * @return Whether this PWM channel is enabled.
         */
        public boolean enabled() {
            switch(mChannel) {
                case 0:
                    return mFlipper.pwm0_enabled();
                case 1:
                    return mFlipper.pwm1_enabled();
                case 2:
                    return mFlipper.pwm2_enabled();
                case 3:
                    return mFlipper.pwm3_enabled();
                default:
                    return false;
            }
        }

        /**
         * Returns whether this PWM channel has interrupts enabled.
         * @return Whether this PWM channel has interrupts enabled.
         */
        public boolean interruptEnabled() {
            switch(mChannel) {
                case 0:
                    return mFlipper.pwm0_interrupt_enabled();
                case 1:
                    return mFlipper.pwm1_interrupt_enabled();
                case 2:
                    return mFlipper.pwm2_interrupt_enabled();
                case 3:
                    return mFlipper.pwm3_interrupt_enabled();
                default:
                    return false;
            }
        }

        /**
         * Returns whether this PWM channel has completed a cycle.
         * @return Whether this PWM channel has completed a cycle.
         */
        public boolean finishedCycle() {
            switch(mChannel) {
                case 0:
                    return mFlipper.pwm0_finished_cycle();
                case 1:
                    return mFlipper.pwm1_finished_cycle();
                case 2:
                    return mFlipper.pwm2_finished_cycle();
                case 3:
                    return mFlipper.pwm3_finished_cycle();
                default:
                    return false;
            }
        }
    }

    /**
     * Connects to a Flipper device via a given communications
     * protocol:
     *
     *      Flipper.SOURCE_USB
     *      Flipper.SOURCE_NETWORK
     *
     * @param connection The interface used to connect to Flipper.
     */
    public void attach(char connection) {
        mFlipper.flipper_attach(connection);
    }

    /**
     * Sets the value of the on-board RGB LED.
     * @param red   The red value, 0-255.
     * @param green The green value, 0-255.
     * @param blue  The blue value, 0-255
     */
    public void setLed(int red, int green, int blue) {
        mFlipper.led_set_rgb(red, green, blue);
    }

    /**
     * Reads the state of Flipper's on-board push button.
     * @return True if pressed, false if released.
     */
    public boolean readButton() {
        return mFlipper.button_read();
    }
}
