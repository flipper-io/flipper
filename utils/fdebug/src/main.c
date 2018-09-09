#include <flipper/flipper.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <libusb.h>

static volatile int alive = 1;

void sigint(int signal) {
    alive = 0;
}

int main(int argc, char *argv[]) {

    struct libusb_context *context = NULL;
    struct libusb_device_handle *handle = NULL;
    int e;
    int len;
    uint8_t buf[DEBUG_IN_SIZE];

    /* capture interrupt signal */
    signal(SIGKILL, sigint);

    e = libusb_init(&context);
    lf_assert(e == 0, E_LIBUSB, "Failed to initialize libusb.");

    handle = libusb_open_device_with_vid_pid(context, FLIPPER_USB_VENDOR_ID, USB_PRODUCT_ID);
    lf_assert(handle, E_LIBUSB, "Failed to find a device.");

    /* Configure USB device. */
    e = libusb_claim_interface(handle, DEBUG_INTERFACE);
    lf_assert(e == 0, E_LIBUSB, "Failed to claim the debug interface.");

    while (alive) {
        uint8_t *_buf = buf;

        e = libusb_interrupt_transfer(handle, DEBUG_IN_ENDPOINT, buf, DEBUG_IN_SIZE, &len, 0);
        lf_assert(e == 0, E_LIBUSB, "Failed to complete transfer.");

        while (len --) printf("%c", *_buf ++);
    }

fail:

    libusb_close(handle);
    libusb_exit(context);

    return 0;
}
