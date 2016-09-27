#ifndef __megausb_h__
#define __megausb_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>
#include <platform/atmega16u2.h>

/* ~ Declare the virtual interface for this driver. ~ */
extern const struct _lf_endpoint megausb;

#ifdef __private_include__

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
extern int megausb_configure(struct _lf_endpoint *endpoint);
extern uint8_t megausb_ready(void);
extern void megausb_put(uint8_t byte);
extern uint8_t megausb_get(void);
extern int megausb_push(void *source, lf_size_t length);
extern int megausb_pull(void *destination, lf_size_t length);
extern int megausb_destroy(struct _lf_endpoint *endpoint);

#define DEFAULT_TIMEOUT                 50
#define MANUFACTURER_STRING             L"Flipper Engineering"
#define PRODUCT_STRING                  L"Flipper: Carbon Edition"

/* USB endpoint configuration macros. */

/* NOTE: Summing the SIZE parameters of each endpoint should be less than or equal to 176. */

#define USB_CONTROL_ENDPOINT			0x00
#define USB_CONTROL_SIZE				16

#define INTERRUPT_TRANSMIT_ENDPOINT		0x01
#define INTERRUPT_TRANSMIT_BUFFER		ENDPOINT_SINGLE_BUFFER
#define INTERRUPT_TRANSMIT_SIZE			32
#define INTERRUPT_TRANSMIT_INTERVAL		1
#define INTERRUPT_RECEIVE_ENDPOINT		0x02
#define INTERRUPT_RECEIVE_BUFFER		ENDPOINT_SINGLE_BUFFER
#define INTERRUPT_RECEIVE_SIZE			64
#define INTERRUPT_RECEIVE_INTERVAL		1

#define BULK_TRANSMIT_ENDPOINT			0x03
#define BULK_TRANSMIT_BUFFER			ENDPOINT_SINGLE_BUFFER
#define BULK_TRANSMIT_SIZE				32
#define BULK_TRANSMIT_INTERVAL			1
#define BULK_RECEIVE_ENDPOINT			0x04
#define BULK_RECEIVE_BUFFER				ENDPOINT_SINGLE_BUFFER
#define BULK_RECEIVE_SIZE				32
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

extern const struct descriptor {
	uint16_t value;
	uint16_t index;
	const uint8_t *address;
	uint8_t length;
} PROGMEM descriptors[DESC_COUNT];

void configure_usb(void);
int8_t interrupt_receive_packet(uint8_t *destination);
int8_t interrupt_transmit_packet(uint8_t *source);

int8_t bulk_receive_packet(uint8_t *destination);
int8_t bulk_transmit_packet(uint8_t *source);

int8_t usb_serial_write(const uint8_t *buffer, uint16_t size);

#endif
#endif
