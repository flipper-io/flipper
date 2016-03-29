#define __private_include__
#include <flipper/platform/hid.h>
#include <flipper/platform/platform.h>

const uint8_t PROGMEM endpoint[] = {

	1, ENDPOINT_TYPE_INTERRUPT_IN, EP_SIZE(HID_TRANSMIT_SIZE) | HID_TRANSMIT_BUFFER,

	1, EP_TYPE_INTERRUPT_OUT, EP_SIZE(HID_RECEIVE_SIZE) | HID_RECEIVE_BUFFER,

	1, ENDPOINT_TYPE_INTERRUPT_IN,  EP_SIZE(DEBUG_TX_SIZE) | DEBUG_TX_BUFFER,

	0

};

static const uint8_t PROGMEM device_descriptor[] = {

	18, 1, 0x00, 0x02, 0, 0, 0, ENDPOINT0_SIZE, lo(CARBON_VENDOR_ID), hi(CARBON_VENDOR_ID), lo(CARBON_PRODUCT_ID), hi(CARBON_PRODUCT_ID), 0x00, 0x01, 1, 2, 0, 1

};

static const uint8_t PROGMEM hid_report[] = {

	0x06, lo(CARBON_USAGE_PAGE), hi(CARBON_USAGE_PAGE),

	0x0A, lo(CARBON_USAGE), hi(CARBON_USAGE),

	0xA1, 0x01,

	0x75, 0x08,

	0x15, 0x00,

	0x26, 0xFF, 0x00,

	0x95, HID_TRANSMIT_SIZE,

	0x09, 0x01,

	0x81, 0x02,

	0x95, HID_RECEIVE_SIZE,

	0x09, 0x02,

	0x91, 0x02,

	0xC0

};

static const uint8_t PROGMEM debug_report[] = {

	0x06, 0x31, 0xFF,			// Usage Page 0xFF31 (vendor defined)

	0x09, 0x74,				// Usage 0x74

	0xA1, 0x53,				// Collection 0x53

	0x75, 0x08,				// report size = 8 bits

	0x15, 0x00,				// logical minimum = 0

	0x26, 0xFF, 0x00,			// logical maximum = 255

	0x95, DEBUG_TX_SIZE,			// report count

	0x09, 0x75,				// usage

	0x81, 0x02,				// Input (array)

	0xC0					// end collection

};


#define CONFIGURATION_SIZE	(9+9+9+7+7+9+9+7)

#define HID_OFFSET			(9+9)

#define DEBUG_OFFSET		(9+9+9+7+7+9)

static const uint8_t PROGMEM configuration[CONFIGURATION_SIZE] = {

	/* HID INTERFACE */

	9,							// bLength

	2,							// bDescriptorType

	lo(CONFIGURATION_SIZE),	// wTotalLength

	hi(CONFIGURATION_SIZE),

	2,							// bNumInterfaces

	1,							// bConfigurationValue

	0,							// iConfiguration

	0xC0,						// bmAttributes

	250,						// bMaxPower

	9,							// bLength

	4,							// bDescriptorType

	HID_INTERFACE,				// bInterfaceNumber

	0,							// bAlternateSetting

	2,							// bNumEndpoints

	0x03,						// bInterfaceClass (0x03 = HID)

	0x00,						// bInterfaceSubClass (0x01 = Boot)

	0x00,						// bInterfaceProtocol (0x01 = Keyboard)

	0,							// iInterface

	9,							// bLength

	0x21,						// bDescriptorType

	0x11, 0x01,					// bcdHID

	0,							// bCountryCode

	1,							// bNumDescriptors

	0x22,						// bDescriptorType

	sizeof(hid_report),				// wDescriptorLength

	0,

	7,							// bLength

	5,							// bDescriptorType

	RAWHID_TX_ENDPOINT | 0x80,	// bEndpointAddress

	0x03,						// bmAttributes (0x03=intr)

	HID_TRANSMIT_SIZE, 0,		// wMaxPacketSize

	HID_TX_INTERVAL,			// bInterval

	7,							// bLength

	5,							// bDescriptorType

	RAWHID_RX_ENDPOINT,			// bEndpointAddress

	0x03,						// bmAttributes (0x03=intr)

	HID_RECEIVE_SIZE, 0,		// wMaxPacketSize

	HID_RX_INTERVAL,			// bInterval

	/* DEBUG INTERFACE */

	9,					// bLength
	4,					// bDescriptorType
	DEBUG_INTERFACE,			// bInterfaceNumber
	0,					// bAlternateSetting
	1,					// bNumEndpoints
	0x03,					// bInterfaceClass (0x03 = HID)
	0x00,					// bInterfaceSubClass
	0x00,					// bInterfaceProtocol
	0,					// iInterface
	// HID interface descriptor, HID 1.11 spec, section 6.2.1
	9,					// bLength
	0x21,					// bDescriptorType
	0x11, 0x01,				// bcdHID
	0,					// bCountryCode
	1,					// bNumDescriptors
	0x22,					// bDescriptorType
	sizeof(debug_report),		// wDescriptorLength
	0,
	// endpoint descriptor, USB spec 9.6.6, page 269-271, Table 9-13
	7,					// bLength
	5,					// bDescriptorType
	DEBUG_TX_ENDPOINT | 0x80,		// bEndpointAddress
	0x03,					// bmAttributes (0x03=intr)
	DEBUG_TX_SIZE, 0,			// wMaxPacketSize
	1					// bInterval


};

struct usb_string {

	uint8_t length;

	uint8_t type;

	int16_t string[];

};

static const struct usb_string PROGMEM language = {

	4, 3, { 0x0409 }

};

static const struct usb_string PROGMEM manufacturer = {

	sizeof(MANUFACTURER_STRING), 3, MANUFACTURER_STRING

};

static const struct usb_string PROGMEM product = {

	sizeof(PRODUCT_STRING), 3, PRODUCT_STRING

};

const struct descriptor PROGMEM descriptors[] = {

	{ 0x0100, 0x0000, device_descriptor, sizeof(device_descriptor) },

	{ 0x0200, 0x0000, configuration, sizeof(configuration) },

	{ 0x2200, HID_INTERFACE, hid_report, sizeof(hid_report) },

	{ 0x2100, HID_INTERFACE, (configuration + HID_OFFSET), 9 },

	{ 0x2200, DEBUG_INTERFACE, debug_report, sizeof(debug_report) },

	{ 0x2100, DEBUG_INTERFACE, (configuration + DEBUG_OFFSET), 9 },

	{ 0x0300, 0x0000, (const uint8_t *)(&language), 4 },

	{ 0x0301, 0x0409, (const uint8_t *)(&manufacturer), sizeof(MANUFACTURER_STRING) },

	{ 0x0302, 0x0409, (const uint8_t *)(&product), sizeof(PRODUCT_STRING) }

};
