#ifndef __gpio_h__
#define __gpio_h__

/* Declare each prototype for all functgpions within this driver. */
int gpio_configure(struct _lf_device *device);
void gpio_enable(struct _lf_device *device, uint32_t enable, uint32_t disable);
void gpio_write(struct _lf_device *device, uint32_t set, uint32_t clear);
uint32_t gpio_read(struct _lf_device *device, uint32_t mask);

#endif
