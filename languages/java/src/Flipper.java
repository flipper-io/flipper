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
    }

    public Io io;

    public Flipper(int endpoint, String name) {
        FLIPPER = (_flipper) Native.loadLibrary("flipper", _flipper.class);
        FLIPPER.flipper_attach_name(endpoint, name);
        io = new Io(FLIPPER);
    }

    public Flipper() {
        FLIPPER = (_flipper) Native.loadLibrary("flipper", _flipper.class);
        FLIPPER.flipper_attach();
        io = new Io(FLIPPER);
    }

    public static void main(String[] args) {
        Flipper flipper = new Flipper();
        flipper.io.direction(7, true);
        flipper.io.write(7, true);

        Flipper java = new Flipper(USB, "java");
        java.io.direction(1, true);
        java.io.write(1, true);
    }
}
