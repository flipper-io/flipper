#ifndef __i2c_h__
#define __i2c_h__

/* Declare the prototypes for all of the functions within this module. */
int i2c_configure(struct _lf_device *device);
void i2c_start_read(struct _lf_device *device, uint8_t address, uint8_t length);
uint8_t i2c_read(struct _lf_device *device);
void i2c_start_write(struct _lf_device *device, uint8_t address, uint8_t length);
void i2c_write(struct _lf_device *device, uint8_t byte);
void i2c_stop(struct _lf_device *device);

#endif
