#include <arpa/inet.h>
#include <flipper/flipper.h>
#include <string.h>
#include <unistd.h>
#define _GNU_SOURCE
#include "posix/network.h"
#include <dlfcn.h>

/* fvm - Creates a local server that acts as a virtual flipper device. */

int main(int argc, char *argv[]) {

    int e;
    int sd;
    struct sockaddr_in addr;
    struct _lf_device *fvm;

    if (argc > 1) {
        if (!strcmp(argv[1], "--version")) {
            printf("FVM version: %s\n\n", lf_get_git_hash());
            return EXIT_SUCCESS;
        }
    }

    lf_set_debug_level(LF_DEBUG_LEVEL_ALL);

    /* Create a UDP server. */
    sd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
    lf_assert(sd, E_UNIMPLEMENTED, "failed to open socket");

    bzero(&addr, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(LF_UDP_PORT);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    e = bind(sd, (struct sockaddr *)&addr, sizeof(addr));
    lf_assert(e == 0, E_UNIMPLEMENTED, "failed to bind socket");

    fvm = lf_device_create(lf_network_read, lf_network_write, lf_network_release);
    lf_assert(fvm, E_ENDPOINT, "failed to create device for virtual machine.");

    fvm->_ep_ctx = calloc(1, sizeof(struct _lf_network_context));
    struct _lf_network_context *context = (struct _lf_network_context *)fvm->_ep_ctx;
    lf_assert(context, E_NULL, "failed to allocate memory for context");
    context->fd = sd;

    lf_attach(fvm);

    printf("Flipper Virtual Machine (FVM) v0.1.0\nListening on 'localhost'.\n\n");

    extern struct _lf_module _adc_module;
    dyld_register(fvm, &_adc_module);
    adc_configure();

    extern struct _lf_module _button_module;
    dyld_register(fvm, &_button_module);
    button_configure();

    extern struct _lf_module _dac_module;
    dyld_register(fvm, &_dac_module);
    dac_configure();

    extern struct _lf_module _gpio_module;
    dyld_register(fvm, &_gpio_module);
    gpio_configure();

    extern struct _lf_module _i2c_module;
    dyld_register(fvm, &_i2c_module);
    i2c_configure();

    extern struct _lf_module _led_module;
    dyld_register(fvm, &_led_module);
    led_configure();

    extern struct _lf_module _pwm_module;
    dyld_register(fvm, &_pwm_module);
    pwm_configure();

    extern struct _lf_module _rtc_module;
    dyld_register(fvm, &_rtc_module);
    rtc_configure();

    extern struct _lf_module _spi_module;
    dyld_register(fvm, &_spi_module);
    spi_configure();

    extern struct _lf_module _swd_module;
    dyld_register(fvm, &_swd_module);
    swd_configure();

    extern struct _lf_module _temp_module;
    dyld_register(fvm, &_temp_module);
    temp_configure();

    extern struct _lf_module _timer_module;
    dyld_register(fvm, &_timer_module);
    timer_configure();

    extern struct _lf_module _uart0_module;
    dyld_register(fvm, &_uart0_module);
    uart0_configure();

    extern struct _lf_module _usart_module;
    dyld_register(fvm, &_usart_module);
    usart_configure();

    extern struct _lf_module _usb_module;
    dyld_register(fvm, &_usb_module);
    usb_configure();

    extern struct _lf_module _wdt_module;
    dyld_register(fvm, &_wdt_module);
    wdt_configure();

    if (argc > 1) {

        char *lib = argv[1];
        char *module, **modules = &argv[2];

        while ((module = *modules++)) {

            printf("Loading module '%s' from '%s'.", module, lib);
            void *dlm = dlopen(lib, RTLD_LAZY);
            lf_assert(dlm, E_NULL, "failed to open module '%s'.", lib);

            char module_sym[32];
            snprintf(module_sym, sizeof(module_sym), "_%s_module", module);

            struct _lf_module *m = dlsym(dlm, module_sym);
            lf_assert(m, E_NULL, "failed to read symbol '%s' from '%s'.", module_sym, lib);
            printf("Successfully loaded symbol '%s'.", module_sym);

            e = dyld_register(fvm, m);
            lf_assert(e, E_NULL, "failed to register module '%s'.", m->name);
            printf("Successfully registered module '%s'.", module);
        }

        printf("\n");
    }

    while (1) {
        struct _fmr_packet packet;
        fvm->read(fvm, &packet, sizeof(packet));
        lf_debug_packet(&packet);
        fmr_perform(fvm, &packet);
    }

    close(sd);

fail:
    return EXIT_FAILURE;
}
