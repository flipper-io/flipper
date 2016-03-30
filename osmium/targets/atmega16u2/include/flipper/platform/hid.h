#ifndef __hid_h__

#define __hid_h__

#include <flipper/core.h>

#include "platform.h"

#ifdef __private_include__

#define DEFAULT_TIMEOUT                 50

#define MANUFACTURER_STRING             L"Flipper Engineering"

#define PRODUCT_STRING                  L"Flipper: Carbon Edition"


#define HID_INTERFACE                   0

#define RAWHID_TX_ENDPOINT              1

#define RAWHID_RX_ENDPOINT              2


#define HID_TRANSMIT_SIZE               64

#define HID_TX_INTERVAL                 1

#define HID_RECEIVE_SIZE                64

#define HID_RX_INTERVAL                 1


#define ENDPOINT0_SIZE                  16


#define DEBUG_INTERFACE                 1

#define DEBUG_TX_ENDPOINT               3

#define DEBUG_TX_SIZE                   32


#define HID_TRANSMIT_BUFFER             EP_SINGLE_BUFFER

#define HID_RECEIVE_BUFFER              EP_SINGLE_BUFFER

#define DEBUG_TX_BUFFER                 EP_SINGLE_BUFFER


#define EP_TYPE_CONTROL                 0x00

#define EP_TYPE_BULK_IN                 0x81

#define EP_TYPE_BULK_OUT                0x80

#define ENDPOINT_TYPE_INTERRUPT_IN      0xC1

#define EP_TYPE_INTERRUPT_OUT           0xC0

#define EP_TYPE_ISOCHRONOUS_IN          0x41

#define EP_TYPE_ISOCHRONOUS_OUT         0x40

#define EP_SINGLE_BUFFER                0x02

#define EP_DOUBLE_BUFFER                0x06

#define EP_SIZE(s)                      ((s) > 32 ? 0x30 : ((s) > 16 ? 0x20 : ((s) > 8  ? 0x10 : 0x00)))

#define MAX_ENDPOINT                    0x04

#define usb_configure_pll()             (PLLCSR = ((1<<PLLE)|(1<<PLLP0)))

#define usb_configure_hardware()        (USBCON = (1<<USBE))

#define usb_freeze()                    (USBCON = ((1<<USBE)|(1<<FRZCLK)))

#define GET_STATUS                      0x00

#define CLEAR_FEATURE                   0x01

#define SET_FEATURE                     0x03

#define SET_ADDRESS                     0x05

#define GET_DESCRIPTOR                  0x06

#define GET_CONFIGURATION               0x08

#define SET_CONFIGURATION               0x09

#define GET_INTERFACE                   0x10

#define SET_INTERFACE                   0x11

#define HID_GET_REPORT                  0x01

#define HID_GET_IDLE                    0x02

#define HID_GET_PROTOCOL                0x03

#define HID_SET_REPORT                  0x09

#define HID_SET_IDLE                    0x010

#define HID_SET_PROTOCOL                0x011

#define NUM_EPCFG                       0x08

#define NUM_DESC_LIST                   0x09

extern const uint8_t PROGMEM endpoint[];

extern const struct descriptor {

	uint16_t value;

	uint16_t index;

	const uint8_t *address;

	uint8_t length;

} PROGMEM descriptors[];

void configure_usb(void);

int8_t usb_receive_packet(uint8_t *buffer);

int8_t usb_send_packet(uint8_t *buffer);

#endif

#endif
