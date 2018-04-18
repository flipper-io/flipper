#ifndef __megausb_h__
#define __megausb_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

extern volatile uint8_t megausb_configuration;

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

#define DEBUG_TRANSMIT_BUFFER			ENDPOINT_SINGLE_BUFFER
#define DEBUG_TRANSMIT_INTERVAL			1

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

#define NUM_DESC_LIST 5

extern const uint8_t PROGMEM endpoint[];

extern const struct descriptor {
	uint16_t value;
	uint16_t index;
	const uint8_t *address;
	uint8_t length;
} PROGMEM descriptors[NUM_DESC_LIST];

int8_t megausb_interrupt_receive(void *dst, uint32_t length);
int8_t megausb_interrupt_transmit(void *src, uint32_t length);

int8_t megausb_bulk_receive(void *dst, uint32_t length);
int8_t megausb_bulk_transmit(void *src, uint32_t length);

int usb_debug_putchar(uint8_t c);

#endif

