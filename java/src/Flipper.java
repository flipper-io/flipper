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

import com.sun.jna.Library;
import com.sun.jna.Native;

/**
 * Created by Nick Mosher on 10/3/15.
 * Flipper's Java Wrapper library.
 */
public class Flipper {

    public static final char SOURCE_USB = (char) 0;
    public static final char SOURCE_NETWORK = (char) 1;

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
    }

    /*
     * In the future when we add support for multiple Flippers to be
     * connected at once, we'll switch to an object-based approach
     * instead of a static one.
     */
    /*public Flipper() {

    }*/

    /**
     * Connects to a Flipper device via a given communications
     * protocol:
     *
     *      Flipper.SOURCE_USB
     *      Flipper.SOURCE_NETWORK
     *
     * @param connection The interface used to connect to Flipper.
     */
    public static void attach(char connection) {
        mFlipper.flipper_attach(connection);
    }

    /**
     * Sets the value of the on-board RGB LED.
     * @param red   The red value, 0-255.
     * @param green The green value, 0-255.
     * @param blue  The blue value, 0-255
     */
    public static void setLed(int red, int green, int blue) {
        mFlipper.led_set_rgb(red, green, blue);
    }

    /**
     * Reads the state of Flipper's on-board push button.
     * @return True if pressed, false if released.
     */
    public static boolean readButton() {
        return mFlipper.button_read();
    }
}