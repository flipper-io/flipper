#ifndef __uart0_h__
#define __uart0_h__

/* Declare the prototypes for all of the functions within this module. */
int uart0_configure(struct _lf_device *device);
int uart0_setbaud(struct _lf_device *device, uint32_t baud);
int uart0_reset(struct _lf_device *device);
int uart0_ready(struct _lf_device *device);
void uart0_put(struct _lf_device *device, const uint8_t byte);
uint8_t uart0_get(struct _lf_device *device);
void uart0_enable(struct _lf_device *device);
void uart0_disable(struct _lf_device *device);
int uart0_write(struct _lf_device *device, void *src, uint32_t length);
int uart0_read(struct _lf_device *device, void *dst, uint32_t length);

#endif
