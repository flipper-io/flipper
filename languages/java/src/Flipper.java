import com.sun.jna.Library;
import com.sun.jna.Native;

/**
 * Created by Nick Mosher on 4/1/16.
 */
public class Flipper {

    private static _flipper FLIPPER;

    public static final int USB = 0;
    public static final int NETWORK = 1;
    public static final int FVM = 2;

    public interface _flipper extends Library {

        int flipper_attach();
        int flipper_attach_name(int endpoint, String name);
        int flipper_select(String name);
        int flipper_detach(String name);

        void io_configure();
        void io_set_direction(byte pin, byte direction);
        void io_write(byte pin, short value);
        short io_read(byte pin);

        void button_configure();
        short button_read();

        void led_configure();
        void led_set_rgb(byte red, byte green, byte blue);
    }

    private _flipper getFLIPPER() {
        if(FLIPPER == null) {
            FLIPPER = (_flipper) Native.loadLibrary("flipper", _flipper.class);
        }
        return FLIPPER;
    }

    public Io io;
    public Button button;
    public Led led;

    /**
     * Created by Nick Mosher on 4/1/16.
     */
    public class Io {

        private Flipper._flipper mFlipper;

        public Io(_flipper flipper) {
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

    /**
     * Wrapper class for Flipper's on-board button.
     */
    public class Button {

        private Flipper._flipper mFlipper;

        public Button(_flipper flipper) {
            mFlipper = flipper;
        }

        public void configure() {
            mFlipper.button_configure();
        }

        public boolean read() {
            return mFlipper.button_read() != 0;
        }
    }

    public Flipper(int endpoint, String name) {
        getFLIPPER().flipper_attach_name(endpoint, name);
        io = new Io(FLIPPER);
    }

    /**
     * Wrapper class for Flipper's on-board RGB LED.
     */
    public class Led {

        private Flipper._flipper mFlipper;

        public Led(_flipper flipper) {
            mFlipper = flipper;
            mFlipper.led_configure();
        }

        public void configure() {
            mFlipper.led_configure();
        }

        public void setRGB(int red, int green, int blue) {
            mFlipper.led_set_rgb((byte) red, (byte) green, (byte) blue);
        }
    }

    /**
     * Constructs an instance of Flipper.
     */
    public Flipper() {
        getFLIPPER().flipper_attach();
        io = new Io(FLIPPER);
        button = new Button(FLIPPER);
        led = new Led(FLIPPER);
    }

    public static void main(String[] args) {
        Flipper flipper = new Flipper();
        flipper.io.direction(1, true);
        flipper.io.write(1, true);
        flipper.led.setRGB((byte) 0, (byte) 0, (byte) 10);

//        try {
//            Thread.sleep(1000);
//        } catch (Exception e) { }

        int value = 0;
        while(true) {
            //flipper.led.setRGB((byte) (value % 255), (byte) ((value + 50) % 255), (byte) ((value + 100) % 255));
            //value++;
            System.out.println("Button: " + (flipper.button.read() ? "Pressed" : "Released"));
            if(flipper.button.read()) flipper.led.setRGB(10, 0, 0); else flipper.led.setRGB(0, 10, 0);
            try {
                Thread.sleep(10);
            } catch(Exception e) { }
        }
    }
}
