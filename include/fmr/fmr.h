#ifndef __fmr_h__

#define __fmr_h__

#include <types.h>

#include <fmr/bus.h>

#define FLIPPER_PACKET_SIZE 32

typedef struct _fmr_packet {
	
	/* ~ Fixed packet header to ensure sync. ~ */
	
	uint8_t header;
	
	/* ~ The destination object. ~ */
	
	uint8_t object;
	
	/* ~ The index of the target method within the destination object. ~ */
	
	uint8_t index;
	
	/* ~ The number of arguments expected by the target method multiplied by sizeof(uint32_t). ~ */
	
	uint8_t argc;
	
	/* ~ The length of the message body. ~ */
	
	uint16_t length;
	
	/* ~ A checksum of the packet. ~ */
	
	uint16_t checksum;
	
	/* ~ The body of the packet. ~ */
	
	uint8_t body[23];
	
} fmr_packet;

/* ~ Declare the object prototype for a generic target: a device that responds to the Flipper Message Runtime. ~ */

struct _target {
	
	void (* configure)(const struct _bus *bus);
	
	uint32_t (* invoke)(uint8_t object, uint8_t index, uint8_t argc, ...);
	
	uint32_t (* push)(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);
	
	void (* pull)(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);
	
	const struct _bus *bus;
	
};

/* ~ Declare the object structure for the 'self' target. ~ */

extern const struct _self {
	
	void (* configure)(const struct _bus *bus);
	
	uint32_t (* invoke)(const struct _target *sender);
	
	uint32_t (* push)(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);
	
	void (* pull)(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);
	
	const struct _bus *bus;
	
} self;

/* ~ Every FMR compliant target will have a host as well as a device. ~ */

extern const struct _target host, device;

extern const void * const objects[];

extern fmr_packet fmrpacket;

#ifdef __private_include__

enum { _button, _flash, _host, _self, _device, _fs, _i2c, _io, _led, _pwm, _spi, _timer, _usart, _usart1, _dbgu, _usb };

extern uint32_t target_invoke(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, va_list *argv);

extern uint32_t target_push(const struct _target *target, uint8_t _object, uint8_t _index, uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, va_list *argv);

extern void target_pull(const struct _target *target, uint8_t _object, uint8_t _index, uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, va_list *argv);


enum { _host_configure, _host_invoke, _host_call, _host_push, _host_pull };

extern void host_configure(const struct _bus *bus);

extern uint32_t host_invoke(uint8_t object, uint8_t index, uint8_t argc, ...);

extern uint32_t host_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);

extern void host_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);


enum { _self_configure, _self_invoke, _self_call, _self_push, _self_pull };

extern void self_configure(const struct _bus *bus);

extern uint32_t self_invoke(const struct _target *sender);

extern uint32_t self_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);

extern void self_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);


enum { _device_configure, _device_invoke, _device_call, _device_push, _device_pull };

extern void device_configure(const struct _bus *bus);

extern uint32_t device_invoke(uint8_t object, uint8_t index, uint8_t argc, ...);

extern uint32_t device_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);

extern void device_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);


extern uint32_t fmr_call(void *function, uint8_t argc, void *argv);

#endif

#endif