/**
 * Wrapper class for Flipper's on-board button.
 */
public class Button {

    private Flipper._flipper mFlipper;

    public Button(Flipper._flipper flipper) {
        mFlipper = flipper;
    }

    public void configure() {
        mFlipper.button_configure();
    }

    public boolean read() {
        return mFlipper.button_read() != 0;
    }
}
