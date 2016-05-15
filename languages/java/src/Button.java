/**
 * Wrapper class for Flipper's on-board button.
 */
public class Button {

    private Flipper mFlipper;

    Button(Flipper flipper) {
        mFlipper = flipper;
    }

    /**
     * Configures the on-board button.
     */
    public void configure() {
        mFlipper.error.clear();
        mFlipper.getBinding().button_configure();
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Reads the state of Flipper's button.
     * @return True if the button is pressed, false otherwise.
     */
    public boolean read() {
        mFlipper.error.clear();
        boolean value = mFlipper.getBinding().button_read() != 0;
        Error.checkErrorCode(mFlipper.error.get());
        return value;
    }
}
