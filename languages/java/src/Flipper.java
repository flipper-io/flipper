import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;

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

        //Flipper filesystem driver bindings.
        void fs_configure();
        void fs_format();
        int fs_data(String name);
        int fs_upload(String path, String name);
        void fs_download(String name, String path);

        //Flipper config driver bindings.
        void config_configure();
        void config_write(byte key, short value);
        short config_read(byte key);

        //Flipper I2c driver bindings.
        void i2c_configure();
        int i2c_put(int mode, int address, Pointer data, int length);
        int i2c_get(int mode, int address, Pointer data, int length);

        //Flipper Usart driver bindings.
        void usart0_configure(Pointer baud);
        void usart0_enable();
        void usart0_disable();
        byte usart0_ready();
        void usart0_put(byte data);
        byte usart0_get();
        void usart0_push(Pointer source, int length);
        void usart0_pull(Pointer destination, int length);

        void usart1_configure(Pointer baud);
        void usart1_enable();
        void usart1_disable();
        byte usart1_ready();
        void usart1_put(byte data);
        byte usart1_get();
        void usart1_push(Pointer source, int length);
        void usart1_pull(Pointer destination, int length);

        void dbgu_configure(Pointer baud);
        void dbgu_enable();
        void dbgu_disable();
        byte dbgu_ready();
        void dbgu_put(byte data);
        byte dbgu_get();
        void dbgu_push(Pointer source, int length);
        void dbgu_pull(Pointer destination, int length);

        //Flipper Usb driver bindings.
        void usb_configure();
        void usb_enable();
        void usb_disable();
        byte usb_ready();
        void usb_put(byte data);
        byte usb_get();
        void usb_push(Pointer source, int length);
        void usb_pull(Pointer destination, int length);

        //Flipper SPI driver bindings.
        void spi_configure();
        void spi_enable();
        void spi_disable();
        byte spi_ready();
        void spi_put(byte data);
        byte spi_get();
        void spi_push(Pointer source, int length);
        void spi_pull(Pointer destination, int length);
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
    public final Fs fs = new Fs(this);
    public final I2c i2c = new I2c(this);
    public final Usart usart0 = new Usart(this, Usart.USART0);
    public final Usart usart1 = new Usart(this, Usart.USART1);
    public final Usart dbgu = new Usart(this, Usart.DGBU);

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
