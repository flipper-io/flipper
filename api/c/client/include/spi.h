#ifndef __spi_h__
#define __spi_h__

/* Declare the prototypes for all of the functions within this module. */
int spi_configure(struct _lf_device *device);
void spi_enable(struct _lf_device *device);
void spi_disable(struct _lf_device *device);
uint8_t spi_ready(struct _lf_device *device);
void spi_put(struct _lf_device *device, uint8_t byte);
void spi_end(struct _lf_device *device);
uint8_t spi_get(struct _lf_device *device);
int spi_write(struct _lf_device *device, void *src, uint32_t length);
int spi_read(struct _lf_device *device, void *dst, uint32_t length);

#endif
