#ifndef __fmr_h__

#define __fmr_h__

#include <flipper/types.h>

#define FLIPPER_DATAGRAM_SIZE 64

#define FMR_BODY_SIZE (FLIPPER_DATAGRAM_SIZE - sizeof(struct _fmr_header) - sizeof(struct _fmr_recipient))

#define FMR_PUSH_PARAMETER_SIZE (5 * 2)

#define fmr_associate_target(target) sender = (struct _target *)(target);

/* ~ It is very important that this structure be packed. ~ */

struct __attribute__((__packed__)) _fmr_header {
	
	/* ~ Fixed packet header to ensure sync. ~ */
	
	uint8_t fe;
	
	/* ~ The length of the contents of the packet. ~ */
	
	uint8_t length;
	
	/* ~ A checksum of everything below. ~ */
	
	uint16_t checksum;
	
};

struct __attribute__((__packed__)) _fmr_recipient {
	
	/* ~ The target device. ~ */
	
	uint8_t target;
	
	/* ~ The destination object. ~ */
	
	uint8_t object;
	
	/* ~ The index of the target method within the destination object. ~ */
	
	uint8_t index;
	
	/* ~ The number of arguments expected by the target method multiplied by sizeof(uint32_t). ~ */
	
	uint8_t argc;
	
};

typedef struct __attribute__((__packed__)) _fmr_packet {
	
	/* ~ The packet header. ~ */
	
	struct _fmr_header header;
	
	/* ~ The packet destination. ~ */
	
	struct _fmr_recipient recipient;
	
	/* ~ The body of the packet. ~ */
	
	uint8_t body[FMR_BODY_SIZE];
	
} fmr_packet;

/* ~ Declare the object prototype for a generic target: a device that responds to the Flipper Message Runtime. ~ */

struct _target {
	
	void (* configure)(const struct _bus *bus);
	
	uint32_t (* call)(void);
	
	uint32_t (* invoke)(uint8_t object, uint8_t index, uint8_t argc, ...);
	
	uint32_t (* push)(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);
	
	uint32_t (* pull)(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);
	
	const struct _bus *bus;
	
	uint8_t id;
	
};

struct _self {
	
	void (* configure)(const struct _bus *bus);
	
	uint32_t (* call)(void);
	
	uint32_t (* invoke)(const struct _target *sender);
	
	uint32_t (* push)(uint8_t object, uint8_t index, uint8_t argc, uint32_t length);
	
	uint32_t (* pull)(uint8_t object, uint8_t index, uint8_t argc, uint32_t length);
	
};

/* ~ Every FMR compliant target will have a host as well as a device. ~ */

extern struct _target host, device;

extern struct _self self;

extern const void * const objects[];

extern fmr_packet fmrpacket;

extern struct _target *sender;

#ifdef __private_include__

extern uint32_t target_invoke(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, va_list *argv);

extern uint32_t target_push(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, va_list *argv);

extern uint32_t target_pull(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, va_list *argv);


enum { _host_configure, _host_call, _host_invoke, _host_push, _host_pull };

extern void host_configure(const struct _bus *bus);

extern uint32_t host_call(void);

extern uint32_t host_invoke(uint8_t object, uint8_t index, uint8_t argc, ...);

extern uint32_t host_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);

extern uint32_t host_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);


enum { _self_configure, _self_call, _self_invoke, _self_push, _self_pull };

extern void self_configure(const struct _bus *bus);

extern uint32_t self_call(void);

extern uint32_t self_invoke(const struct _target *sender);

extern uint32_t self_push(uint8_t object, uint8_t index, uint8_t argc, uint32_t length);

extern uint32_t self_pull(uint8_t object, uint8_t index, uint8_t argc, uint32_t length);


enum { _device_configure, _device_call, _device_invoke, _device_push, _device_pull };

extern void device_configure(const struct _bus *bus);

extern uint32_t device_call(void);

extern uint32_t device_invoke(uint8_t object, uint8_t index, uint8_t argc, ...);

extern uint32_t device_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);

extern uint32_t device_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);


extern uint32_t fmr_call(void);

extern uint32_t fmr_invoke(const struct _target *sender);

extern uint32_t internal_call(void *function, uint8_t argc, void *argv);

extern bool fmr_busy;

extern void fmr_broadcast(void);

extern void fmr_retrieve(void);

#endif

#endif
