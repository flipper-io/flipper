#ifndef __usart_h__
#define __usart_h__

/* Declare the prototypes for all of the functions within this module. */
int usart_configure(struct _lf_device *device);
int usart_ready(struct _lf_device *device);
void usart_enable(struct _lf_device *device);
void usart_disable(struct _lf_device *device);
void usart_put(struct _lf_device *device, uint8_t byte);
uint8_t usart_get(struct _lf_device *device);
int usart_write(struct _lf_device *device, void *src, uint32_t length);
int usart_read(struct _lf_device *device, void *dst, uint32_t length);

#endif
