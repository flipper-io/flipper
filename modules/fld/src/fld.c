#define __private_include__
#include <flipper/fld.h>
#include <flipper/fs.h>

#ifdef __use_fld__
/* Define the virtual interface for this module. */
const struct _fld fld = {
	fld_configure,
	fld_bind,
#ifdef __fld_upload_symbols__
	fld_load
#endif
};
#endif

fmr_module fld_bind(lf_id_t identifier) {
	return 0;
}

#ifdef __fld_upload_symbols__

#include <dlfcn.h>

/* Uploads the module to the device and registers the interface. */
int fld_load(char *path, void **interface) {
	void *handle = dlopen(path, RTLD_LAZY);
	if (!handle) {
		error_raise(E_MODULE, error_message("%s", dlerror()));
		return lf_error;
	}
	/* Obtain a pointer to the module. */
	struct _lf_module **module = dlsym(handle, "__fld_module_address");
	char *error;
	if ((error = dlerror()) != NULL)  {
		error_raise(E_MODULE, error_message("%s", error));
		return lf_error;
	}
	/* Extract the loadable code from the module. */
	void *code = dlsym(handle, "device_bin");
	if ((error = dlerror()) != NULL)  {
		error_raise(E_MODULE, error_message("%s", error));
		return lf_error;
	}
	/* Get the size of the loadable code from the module. */
	uint32_t *size = dlsym(handle, "device_bin_len");
	if ((error = dlerror()) != NULL)  {
		error_raise(E_MODULE, error_message("%s", error));
		return lf_error;
	}
	/* Delete the module from the device if it already exists. */
	suppress_errors(fs_delete((*module) -> name));
	/* Create the module on the device. */
	int _e = fs_create((*module) -> name, *size);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Open the newly created file. */
	_e = fs_open((*module) -> name, 0);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Load the code into the module. */
	fs_push(code, *size);
	/* Close the file. */
	fs_close();
	/* Set the file attributes. */

	/* Load the interface. */
	*interface = dlsym(handle, (*module) -> name);
	if ((error = dlerror()) != NULL)  {
		error_raise(E_MODULE, error_message("%s", error));
		return lf_error;
	}
	return lf_success;
}

#endif
