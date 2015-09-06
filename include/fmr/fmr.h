#ifndef __fmr_h__

#define __fmr_h__

#include <flipper/types.h>

#define FLIPPER_PACKET_SIZE 32

struct _target {
	
	void (* configure)(void);
	
	uint32_t (* invoke)(uint8_t module, uint8_t index, uint8_t argc, ...);
	
	uint32_t (* push)(uint8_t module, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);
	
	void (* pull)(uint8_t module, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);
	
	const struct _bus *bus;
	
};

extern const struct _target host, self, device;

extern uint8_t fmr_buffer[FLIPPER_PACKET_SIZE];

extern const void *system_modules[];

#ifdef __private_include__

enum { _button, _flash, _host, _self, _device, _fs, _i2c, _io, _led, _pwm, _spi, _timer, _usart, _usb };

extern uint32_t target_invoke(const struct _target *target, uint8_t module, uint8_t index, uint8_t argc, va_list *argv);

extern uint32_t target_push(const struct _target *target, uint8_t _module, uint8_t _index, uint8_t module, uint8_t index, uint8_t argc, void *source, uint32_t length, va_list *argv);

extern void target_pull(const struct _target *target, uint8_t _module, uint8_t _index, uint8_t module, uint8_t index, uint8_t argc, void *destination, uint32_t length, va_list *argv);


enum { _host_configure, _host_invoke, _host_call, _host_push, _host_pull };

extern void host_configure(void);

extern uint32_t host_invoke(uint8_t module, uint8_t index, uint8_t argc, ...);

extern uint32_t host_push(uint8_t module, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);

extern void host_pull(uint8_t module, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);


enum { _self_configure, _self_invoke, _self_call, _self_push, _self_pull };

extern void self_configure(void);

extern uint32_t self_invoke(uint8_t module, uint8_t index, uint8_t argc, ...);

extern uint32_t internal_call(void *function, uint8_t argc, void *argv);

extern uint32_t self_push(uint8_t module, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);

extern void self_pull(uint8_t module, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);


enum { _device_configure, _device_invoke, _device_call, _device_push, _device_pull };

extern void device_configure(void);

extern uint32_t device_invoke(uint8_t module, uint8_t index, uint8_t argc, ...);

extern uint32_t device_push(uint8_t module, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);

extern void device_pull(uint8_t module, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);

#endif

#endif