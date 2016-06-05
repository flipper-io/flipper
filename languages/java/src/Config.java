/**
 * Created by Nick Mosher on 5/31/16.
 */
public class Config {

    private Flipper mFlipper;

    Config(Flipper flipper) {
        mFlipper = flipper;
    }

    void configure() {
        mFlipper.error.clear();
        mFlipper.getBinding().config_configure();
        Error.checkErrorCode(mFlipper.error.get());
    }

    void write(byte key, short value) {
        mFlipper.error.clear();
        mFlipper.getBinding().config_write(key, value);
        Error.checkErrorCode(mFlipper.error.get());
    }

    short read(byte key) {
        mFlipper.error.clear();
        short config = mFlipper.getBinding().config_read(key);
        Error.checkErrorCode(mFlipper.error.get());
        return config;
    }
}
