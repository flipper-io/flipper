/**
 * Wrapper class for Flipper's on-board RGB LED.
 */
public class Led {

    private Flipper._flipper mFlipper;

    public Led(Flipper._flipper flipper) {
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
