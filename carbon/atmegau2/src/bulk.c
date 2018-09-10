#include "libflipper.h"
#include "atmegau2.h"
#include "megausb.h"

#define LF_USB_TIMEOUT 3

int megausb_bulk_receive(void *dst, uint32_t length) {

    uint8_t _sreg = SREG;

    if (!megausb_configuration) return lf_error;

    cli();

    /* Select the endpoint that has been configured to receive bulk data. */
    UENUM = BULK_OUT_ENDPOINT;

    while (length) {

        /* Wait until the USB controller is ready. */
        uint8_t timeout = UDFNUML + LF_USB_TIMEOUT;

        while (1) {

            if (UEINTX & (1 << RWAL)) break;

            SREG = _sreg;
            if (UDFNUML >= timeout) return lf_error;
            if (!megausb_configuration) return lf_error;
            _sreg = SREG;
            cli();

            UENUM = BULK_OUT_ENDPOINT;
        }

        /* calculate send length */
        size_t len = (length > BULK_OUT_SIZE) ? BULK_OUT_SIZE : length;
        length -= len;

        lf_debug("reading %i bytes\n", len);

        /* fill the fifo with the available data */
        for (size_t i = 0; i < len; i++) *(uint8_t *)dst++ = UEDATX;
        /* clear the fifo */
        UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);
    }

    SREG = _sreg;

    return lf_success;
}

int megausb_bulk_transmit(void *src, uint32_t length) {

    uint8_t _sreg = SREG;

    if (!megausb_configuration) return lf_error;

    cli();

    /* Select the endpoint that has been configured to receive bulk data. */
    UENUM = BULK_IN_ENDPOINT & ~USB_IN_MASK;

    lf_debug("sending %i from bulk", length);

    while (length) {

        /* Wait until the USB controller is ready. */
        uint8_t timeout = UDFNUML + LF_USB_TIMEOUT;

        while (1) {

            if (UEINTX & (1 << RWAL)) break;

            SREG = _sreg;
            if (UDFNUML >= timeout) return lf_error;
            if (!megausb_configuration) return lf_error;
            _sreg = SREG;
            cli();

            UENUM = BULK_IN_ENDPOINT & ~USB_IN_MASK;
        }

        /* calculate send length */
        size_t len = (length > BULK_IN_SIZE) ? BULK_IN_SIZE : length;
        length -= len;

        lf_debug("writing %i bytes\n", len);

        /* fill the fifo with the available data */
        for (size_t i = 0; i < len; i++) UEDATX = *(uint8_t *)src++;
        /* shift the fifo out */
        UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);
    }

    SREG = _sreg;

    return lf_success;
}
