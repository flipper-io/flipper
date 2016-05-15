/**
 * Wrapper class for Flipper's on-board RGB LED.
 */
public class Led {

    private Flipper mFlipper;

    Led(Flipper flipper) {
        mFlipper = flipper;
    }

    /**
     * Configures the on-board RBG LED.
     */
    public void configure() {
        mFlipper.error.clear();
        mFlipper.getBinding().led_configure();
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Sets the color of Flipper's RGB LED.
     * @param red The component of red, 0-255.
     * @param green The component of green, 0-255.
     * @param blue The component of blue, 0-255.
     */
    public void setRGB(int red, int green, int blue) {
        mFlipper.error.clear();
        mFlipper.getBinding().led_set_rgb((byte) red, (byte) green, (byte) blue);
        Error.checkErrorCode(mFlipper.error.get());
    }
}
