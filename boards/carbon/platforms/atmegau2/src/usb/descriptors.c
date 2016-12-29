#define __private_include__
#include <private/megausb.h>

const uint8_t PROGMEM endpoint[] = {
	/* Bidirectional interrupt endpoints for quick FMR transactions. */
	1, ENDPOINT_TYPE_INTERRUPT_IN, ENDPOINT_SIZE(INTERRUPT_IN_SIZE) | INTERRUPT_TRANSMIT_BUFFER,
	1, ENDPOINT_TYPE_INTERRUPT_OUT, ENDPOINT_SIZE(INTERRUPT_OUT_SIZE) | INTERRUPT_RECEIVE_BUFFER,
	/* Bidirectional bulk endpoints for large push/pull exchanges. */
	1, ENDPOINT_TYPE_BULK_IN, ENDPOINT_SIZE(BULK_IN_SIZE) | BULK_TRANSMIT_BUFFER,
	1, ENDPOINT_TYPE_BULK_OUT, ENDPOINT_SIZE(BULK_OUT_SIZE) | BULK_RECEIVE_BUFFER
};

static const uint8_t PROGMEM device_descriptor[] = {
	18,					// bLength
	1,					// bDescriptorType
	0x00, 0x02,			// bcdUSB
	VENDOR_SPECIFIC,	// bDeviceClass
	0,					// bDeviceSubClass
	0,					// bDeviceProtocol
	USB_CONTROL_SIZE,	// bMaxPacketSize
	lo(USB_VENDOR_ID), hi(USB_VENDOR_ID),	// idVendor
	lo(USB_PRODUCT_ID), hi(USB_PRODUCT_ID),	// idProduct
	0x00, 0x01,			// bcdDevice
	1,					// iManufacturer
	2,					// iProduct
	0,					// iSerialNumber
	1					// bNumConfigurations
};

#define CONFIGURATION_SIZE	(9+9+7+7+7+7)

static const uint8_t PROGMEM configuration[CONFIGURATION_SIZE] = {
	/* Configuration descriptor. (USB spec 9.6.3, page 264-266, Table 9-10) */
	0x09,						// bLength
	0x02,						// bDescriptorType
	lo(CONFIGURATION_SIZE),		// wTotalLength
	hi(CONFIGURATION_SIZE),
	0x01,						// bNumInterfaces
	0x01,						// bConfigurationValue
	0x00,						// iConfiguration
	0xC0,						// bmAttributes
	250,						// bMaxPower

	/* Interface 0 (interrupt) descriptor. (USB spec 9.6.5, page 267-269, Table 9-12) */
	0x09,						// bLength
	0x04,						// bDescriptorType
	0,							// bInterfaceNumber
	0x00,						// bAlternateSetting
	0x04,						// bNumEndpoints
	VENDOR_SPECIFIC,			// bInterfaceClass
	0x01,						// bInterfaceSubClass
	0x01,						// bInterfaceProtocol
	0x00,						// iInterface

	/* Interrupt IN endpoint descriptor. (USB spec 9.6.6, page 269-271, Table 9-13) */
	0x07,						// bLength
	0x05,						// bDescriptorType
	INTERRUPT_IN_ENDPOINT,		// bEndpointAddress
	0x03,						// bmAttributes (0x03=intr)
	INTERRUPT_IN_SIZE, 0x00,	// wMaxPacketSize
	INTERRUPT_TRANSMIT_INTERVAL,	// bInterval

	/* Interrupt OUT endpoint descriptor. (USB spec 9.6.6, page 269-271, Table 9-13) */
	0x07,						// bLength
	0x05,						// bDescriptorType
	INTERRUPT_OUT_ENDPOINT,		// bEndpointAddress
	0x03,						// bmAttributes (0x03=intr)
	INTERRUPT_OUT_SIZE, 0x00,	// wMaxPacketSize
	INTERRUPT_RECEIVE_INTERVAL,	// bInterval

//	/* Interface 1 (bulk) descriptor. (USB spec 9.6.5, page 267-269, Table 9-12) */
//	0x09,						// bLength
//	0x04,						// bDescriptorType
//	1,							// bInterfaceNumber
//	0x00,						// bAlternateSetting
//	0x02,						// bNumEndpoints
//	VENDOR_SPECIFIC,			// bInterfaceClass
//	0x01,						// bInterfaceSubClass
//	0x01,						// bInterfaceProtocol
//	0x00,						// iInterface

	/* Bulk IN endpoint descriptor. (USB spec 9.6.6, page 269-271, Table 9-13) */
	0x07,						// bLength
	0x05,						// bDescriptorType
	BULK_IN_ENDPOINT, 		// bEndpointAddress
	0x02,						// bmAttributes (0x02 = bulk)
	BULK_IN_SIZE, 0x00,	// wMaxPacketSize
	BULK_TRANSMIT_INTERVAL,		// bInterval

	/* Bulk OUT endpoint descriptor. (USB spec 9.6.6, page 269-271, Table 9-13) */
	0x07,						// bLength
	0x05,						// bDescriptorType
	BULK_OUT_ENDPOINT,			// bEndpointAddress
	0x02,						// bmAttributes (0x02 = bulk)
	BULK_OUT_SIZE, 0x00,		// wMaxPacketSize
	BULK_RECEIVE_INTERVAL		// bInterval
};

struct usb_string {
	uint8_t length;
	uint8_t type;
	int16_t string[];
};

static const struct usb_string PROGMEM language = {
	0x04, 0x03, { 0x0409 }
};

static const struct usb_string PROGMEM manufacturer = {
	sizeof(MANUFACTURER_STRING), 0x03, MANUFACTURER_STRING
};

static const struct usb_string PROGMEM product = {
	sizeof(PRODUCT_STRING), 0x03, PRODUCT_STRING
};

const struct descriptor PROGMEM descriptors[] = {
	{ 0x0100, 0x0000, device_descriptor, sizeof(device_descriptor) },
	{ 0x0200, 0x0000, configuration, sizeof(configuration) },
	{ 0x0300, 0x0000, (const uint8_t *)(&language), 4 },
	{ 0x0301, 0x0409, (const uint8_t *)(&manufacturer), sizeof(MANUFACTURER_STRING) },
	{ 0x0302, 0x0409, (const uint8_t *)(&product), sizeof(PRODUCT_STRING) },
};
