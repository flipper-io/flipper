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

    public Flipper(int endpoint, String name) {
        getFLIPPER().flipper_attach_name(endpoint, name);
        io = new Io(FLIPPER);
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
