package io.flipper.java.drivers;

import java.io.File;
import java.lang.*;

/**
 * Created by Nick Mosher on 5/31/16.
 */
public class Fs {

    private Flipper mFlipper;

    /**
     * Initialize the filesystem wrapper with an instance of Flipper.
     * @param flipper The active instance of Flipper.
     */
    Fs(Flipper flipper) {
        mFlipper = flipper;
    }

    /**
     * Represents a file on Flipper's filesystem.
     */
    public static class FilePointer {

        private int mAddress;

        /**
         * Default-access constructor so that only Flipper library functions
         * can create FilePointers to ensure they're valid.
         * @param address The address of the file on Flipper's filesystem.
         */
        FilePointer(int address) {
            mAddress = address;
        }

        /**
         * @return The raw address of the file on Flipper's filesystem.
         */
        public int getRawAddress() {
            return mAddress;
        }
    }

    /**
     * Configures Flipper's filesystem.
     */
    public void configure() {
        mFlipper.error.clear();
        mFlipper.getBinding().fs_configure();
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Formats Flipper's filesystem, erasing all existing data.
     */
    public void format() {
        mFlipper.error.clear();
        mFlipper.getBinding().fs_format();
        Error.checkErrorCode(mFlipper.error.get());
    }

    /**
     * Search for a file named `name` on Flipper's filesystem,
     * returning the pointer address of the file if found.
     * @param name The name of the file to search for.
     * @return The FilePointer of the file on Flipper.
     */
    public FilePointer data(String name) {
        mFlipper.error.clear();
        int address = mFlipper.getBinding().fs_data(name);
        Error.checkErrorCode(mFlipper.error.get());
        return new FilePointer(address);
    }

    /**
     * Uploads the given file from the local system to Flipper's filesystem.
     * @param file The file to upload.
     * @return A FilePointer to the uploaded file in Flipper's filesystem.
     */
    public FilePointer upload(File file) {
        return upload(file.getAbsolutePath(), file.getName());
    }

    /**
     * Uploads a file from the local machine at `path` named
     * `name` to Flipper's filesystem.
     * @param path The path to the file on the local system.
     * @param name The name of the file at `path` to upload.
     * @return A FilePointer to the uploaded file in Flipper's filesystem.
     */
    public FilePointer upload(String path, String name) {
        mFlipper.error.clear();
        int address = mFlipper.getBinding().fs_upload(path, name);
        Error.checkErrorCode(mFlipper.error.get());
        return new FilePointer(address);
    }

    /**
     * Retrieves a file named `name` from Flipper's filesystem
     * and downloads it to `path` on the local system.
     * @param name The name of the file to download.
     * @param path The path on the local system to save the file.
     */
    public void download(String name, String path) {
        mFlipper.error.clear();
        mFlipper.getBinding().fs_download(name, path);
        Error.checkErrorCode(mFlipper.error.get());
    }
}
