#ifndef __megausb_h__
#define __megausb_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/libflipper.h>
#include <flipper/atmegau2/atmegau2.h>

/* ~ Declare the virtual interface for this driver. ~ */
extern struct _lf_endpoint megausb;

extern volatile uint8_t megausb_configured;

#ifdef __private_include__

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
extern int megausb_configure();
extern uint8_t megausb_ready(struct _lf_endpoint *this);
extern void megausb_put(struct _lf_endpoint *this, uint8_t byte);
extern uint8_t megausb_get(struct _lf_endpoint *this);
extern int megausb_push(struct _lf_endpoint *this, void *source, lf_size_t length);
extern int megausb_pull(struct _lf_endpoint *this, void *destination, lf_size_t length);
extern int megausb_destroy();

/* USB endpoint configuration macros. */

#define USB_CONTROL_ENDPOINT			0x00
#define USB_CONTROL_SIZE				16

#define INTERRUPT_TRANSMIT_BUFFER		ENDPOINT_SINGLE_BUFFER
#define INTERRUPT_TRANSMIT_INTERVAL		1
#define INTERRUPT_RECEIVE_BUFFER		ENDPOINT_SINGLE_BUFFER
#define INTERRUPT_RECEIVE_INTERVAL		1

#define BULK_TRANSMIT_BUFFER			ENDPOINT_SINGLE_BUFFER
#define BULK_TRANSMIT_INTERVAL			1
#define BULK_RECEIVE_BUFFER				ENDPOINT_SINGLE_BUFFER
#define BULK_RECEIVE_INTERVAL			1

#define ENDPOINT_TYPE_CONTROL			0x00
#define ENDPOINT_TYPE_BULK_IN			0x81
#define ENDPOINT_TYPE_BULK_OUT			0x80
#define ENDPOINT_TYPE_INTERRUPT_IN		0xC1
#define ENDPOINT_TYPE_INTERRUPT_OUT		0xC0
#define ENDPOINT_TYPE_ISOCHRONOUS_IN	0x41
#define ENDPOINT_TYPE_ISOCHRONOUS_OUT	0x40
#define ENDPOINT_SINGLE_BUFFER			0x02
#define ENDPOINT_DOUBLE_BUFFER			0x06
#define ENDPOINT_SIZE(s)				((s) > 32 ? 0x30 : ((s) > 16 ? 0x20 : ((s) > 8  ? 0x10 : 0x00)))

#define MAX_ENDPOINT					0x04

#define GET_STATUS                      0x00
#define CLEAR_FEATURE                   0x01
#define SET_FEATURE                     0x03
#define SET_ADDRESS                     0x05
#define GET_DESCRIPTOR                  0x06
#define GET_CONFIGURATION               0x08
#define SET_CONFIGURATION               0x09
#define GET_INTERFACE                   0x10
#define SET_INTERFACE                   0x11

#define VENDOR_SPECIFIC					0xFF

#define NUM_DESC_LIST (sizeof(descriptors) / sizeof(struct descriptor))

extern const uint8_t PROGMEM endpoint[];

#define DESC_COUNT 5

/* Timeout using TIMER1. Waits ~100ms. */
#define megausb_start_timeout() \
    TCCR1B |= (1 << WGM12); \
    OCR1A = 6250; \
    TCCR1B |= (1 << CS12) | (0 << CS11) | (0 << CS10);
#define megausb_is_timed_out() \
	(TIFR1 & (1 << OCF1A))
#define megausb_stop_timeout() \
	TIFR1 |= (1 << OCF1A); \
	TCCR1B = 0;

extern const struct descriptor {
	uint16_t value;
	uint16_t index;
	const uint8_t *address;
	uint8_t length;
} PROGMEM descriptors[DESC_COUNT];

void configure_usb(void);
int8_t megausb_interrupt_receive(uint8_t *destination, lf_size_t length);
int8_t megausb_interrupt_transmit(uint8_t *source, lf_size_t length);

int8_t megausb_bulk_receive(uint8_t *destination, lf_size_t length);
int8_t megausb_bulk_transmit(uint8_t *source, lf_size_t length);

int8_t usb_serial_write(const uint8_t *buffer, uint16_t size);

int megausb_wait_ready(void);

#endif
#endif
