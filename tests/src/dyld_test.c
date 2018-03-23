/* dyld_test tests the dynamic loader */

#include <flipper.h>
#include <tests.h>

int test_func(void) {
    printf("derp\n");
    return lf_success;
}

int dyld_test(void) {

    struct _lf_module *module = (void *)0xdeadbeef;
    lf_try(module = lf_module_create(NULL, 0));
    lf_expect_error();
    lf_assert(module == NULL, failure, E_UNIMPLEMENTED, "Module was not NULL.");
    lf_try(lf_module_release(module));
    lf_expect_error();

    lf_try(module = lf_module_create("test", 1));
    lf_expect_success();
    lf_assert(module, failure, E_UNIMPLEMENTED, "Failed to create module.");
    lf_assert(!strcmp("test", module->name), failure, E_UNIMPLEMENTED, "Module name doesn't match.");
    lf_assert(module->idx == 1, failure, E_UNIMPLEMENTED, "Module name doesn't match.");

    struct _lf_endpoint *endpoint = (void *)0xdeadbeef;
    lf_try(endpoint = lf_endpoint_create(NULL, NULL, NULL, NULL, NULL, 0));
    lf_expect_error();
    lf_assert(endpoint == NULL, failure, E_UNIMPLEMENTED, "Endpoint was not NULL.");
    lf_try(lf_endpoint_release(endpoint));
    lf_expect_error();

    lf_try(endpoint = lf_endpoint_create((void *)&test_func, (void *)&test_func, (void *)&test_func, (void *)&test_func, (void *)&test_func, 0));
    lf_expect_success();
    lf_assert(endpoint, failure, E_UNIMPLEMENTED, "Endpoint was NULL.");

    struct _lf_device *device = (void *)0xdeadbeef;
    lf_try(device = lf_device_create(NULL, NULL));
    lf_expect_error();
    lf_assert(device == NULL, failure, E_UNIMPLEMENTED, "Device was not NULL.");

    lf_try(device = lf_device_create("test", endpoint));
    lf_expect_success();
    lf_assert(device, failure, E_UNIMPLEMENTED, "Device was NULL.");
    lf_assert(!strcmp("test", device->name), failure, E_UNIMPLEMENTED, "Device name doesn't match.");
    lf_assert(device->endpoint == endpoint, failure, E_UNIMPLEMENTED, "Device endpoint pounter doesn't match.");

    lf_assert(device->modules == NULL, failure, E_UNIMPLEMENTED, "Device modules are not NULL.");
    lf_try(dyld_register(NULL, NULL));
    lf_expect_error();
    lf_try(dyld_register(device, module));
    lf_expect_success();
    lf_assert(device->modules, failure, E_UNIMPLEMENTED, "Device modules were NULL.");

    /* Try to get an invalid module. */
    struct _lf_module *loaded = NULL;
    lf_try(loaded = dyld_module(device, "nope"));
    lf_expect_error();
    lf_assert(loaded == NULL, failure, E_UNIMPLEMENTED, "Loaded module was not NULL.");

    lf_try(loaded = dyld_module(device, "test"));
    lf_expect_success();
    lf_assert(loaded == module, failure, E_UNIMPLEMENTED, "Loaded module did not mach.");

    lf_device_release(device);
    lf_expect_success();

    return lf_success;
failure:
    return lf_error;
}
