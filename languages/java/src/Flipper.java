import com.sun.jna.Library;
import com.sun.jna.Native;

/**
 * Created by Nick Mosher on 4/1/16.
 */
public class Flipper {

    public static final int USB = 0;
    public static final int NETWORK = 1;
    public static final int FVM = 2;

    private static _binding BINDING;

    interface _binding extends Library {

        //Flipper attach driver bindings.
        int flipper_attach();
        int flipper_attach_name(int endpoint, String name);
        int flipper_select(String name);
        int flipper_detach(String name);

        //Flipper error driver bindings.
        void error_configure();
        void error_withold();
        void error_disclose();
        void error_raise(short code, String message);
        short error_get();
        void error_clear();

        //Flipper io driver bindings.
        void io_configure();
        void io_set_direction(byte pin, byte direction);
        void io_write(byte pin, short value);
        short io_read(byte pin);

        //Flipper button driver bindings.
        void button_configure();
        short button_read();

        //Flipper led driver bindings.
        void led_configure();
        void led_set_rgb(byte red, byte green, byte blue);
    }

    /**
     * Default-access method allows driver-wrapping classes in this package to get
     * the instance of the Flipper bindings and execute them, but no external-scope
     * methods can.
     * @return An inflated interface of Java methods bound to underlying C functions.
     */
    _binding getBinding() {
        if(BINDING == null) {
            BINDING = (_binding) Native.loadLibrary("flipper", _binding.class);
        }
        return BINDING;
    }

    /*
     * Instantiate each driver wrapper.
     */
    public final Io io = new Io(this);
    public final Button button = new Button(this);
    public final Led led = new Led(this);
    public final Error error = new Error(this);

    /**
     * Constructs a Flipper object bound to a Flipper with the given name
     * over the given endpoint (USB, NETWORK, or FVM (Flipper Virtual Machine)).
     * @param endpoint The communication endpoint to connect to Flipper with.
     * @param name The name of the Flipper device to connect to.
     */
    public Flipper(int endpoint, String name) {
        getBinding().flipper_attach_name(endpoint, name);
    }

    /**
     * Constructs an instance of Flipper.
     */
    public Flipper() {
        getBinding().flipper_attach();
    }
}
