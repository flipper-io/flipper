#include <flipper.h>

/* Expose the virtual interface for this driver. */
const struct _flipper_interface flipper = {
	flipper_attach,
	flipper_select,
	flipper_detach,
	flipper_exit,
};

/* Shim to attach all possible flipper devices that could be attached to the system. */
struct _lf_device *flipper_attach(void) {
	lf_assert(carbon_attach(), E_NO_DEVICE, "Failed to find any Flipper devices attached to this computer. Please check your connection and try again.");

	return lf_get_selected();
fail:
	return NULL;
}

int flipper_select(struct _lf_device *device) {
	return lf_select(device);
}

/* Shim around lf_detach. */
int flipper_detach(struct _lf_device *device) {
	return lf_detach(device);
}

/* Shim around lf_exit. */
int flipper_exit(void) {
	return lf_exit();
}
