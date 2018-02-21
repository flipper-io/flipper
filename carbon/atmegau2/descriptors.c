#include <flipper/atmegau2/megausb.h>

const uint8_t PROGMEM endpoint[] = {
	1, ENDPOINT_TYPE_BULK_IN, ENDPOINT_SIZE(BULK_IN_SIZE) | BULK_TRANSMIT_BUFFER,
	1, ENDPOINT_TYPE_BULK_OUT, ENDPOINT_SIZE(BULK_OUT_SIZE) | BULK_RECEIVE_BUFFER,
	1, ENDPOINT_TYPE_INTERRUPT_IN, ENDPOINT_SIZE(DEBUG_IN_SIZE) | DEBUG_TRANSMIT_BUFFER,
};

static const uint8_t PROGMEM device_descriptor[] = {
	18,							// bLength
	1,							// bDescriptorType
	0x00, 0x02,					// bcdUSB
	VENDOR_SPECIFIC,			// bDeviceClass
	0,							// bDeviceSubClass
	0,							// bDeviceProtocol
	USB_CONTROL_SIZE,			// bMaxPacketSize
	lo(CARBON_USB_VENDOR_ID),	// idVendor LO
	hi(CARBON_USB_VENDOR_ID),	// idVendor HI
	lo(CARBON_USB_PRODUCT_ID),	// idProduct LO
	hi(CARBON_USB_PRODUCT_ID),	// idProduct HI
	0x00, 0x01,					// bcdDevice
	1,							// iManufacturer
	2,							// iProduct
	0,							// iSerialNumber
	1							// bNumConfigurations
};

#define CONFIGURATION_SIZE	(9+9/*+7+7*/+7+7+9+7)

static const uint8_t PROGMEM configuration[CONFIGURATION_SIZE] = {
	/* Configuration descriptor. (USB spec 9.6.3, page 264-266, Table 9-10) */
	0x09,						// bLength
	0x02,						// bDescriptorType
	lo(CONFIGURATION_SIZE), 	// wTotalLength LO
	hi(CONFIGURATION_SIZE), 	// wTotalLength HI
	0x01,						// bNumInterfaces
	0x01,						// bConfigurationValue
	0x00,						// iConfiguration
	0xC0,						// bmAttributes
	250,						// bMaxPower

	/* Interface 0 descriptor. (USB spec 9.6.5, page 267-269, Table 9-12) */
	0x09,						// bLength
	0x04,						// bDescriptorType
	FMR_INTERFACE,				// bInterfaceNumber
	0x00,						// bAlternateSetting
	0x02,						// bNumEndpoints
	VENDOR_SPECIFIC,			// bInterfaceClass
	0x01,						// bInterfaceSubClass
	0x01,						// bInterfaceProtocol
	0x00,						// iInterface

	// /* Interrupt IN endpoint descriptor. (USB spec 9.6.6, page 269-271, Table 9-13) */
	// 0x07,						// bLength
	// 0x05,						// bDescriptorType
	// INTERRUPT_IN_ENDPOINT,		// bEndpointAddress
	// 0x03,						// bmAttributes (0x03=intr)
	// INTERRUPT_IN_SIZE, 0x00,	// wMaxPacketSize
	// INTERRUPT_TRANSMIT_INTERVAL,	// bInterval
	//
	// /* Interrupt OUT endpoint descriptor. (USB spec 9.6.6, page 269-271, Table 9-13) */
	// 0x07,						// bLength
	// 0x05,						// bDescriptorType
	// INTERRUPT_OUT_ENDPOINT,		// bEndpointAddress
	// 0x03,						// bmAttributes (0x03=intr)
	// INTERRUPT_OUT_SIZE, 0x00,	// wMaxPacketSize
	// INTERRUPT_RECEIVE_INTERVAL,	// bInterval

	/* Bulk IN endpoint descriptor. (USB spec 9.6.6, page 269-271, Table 9-13) */
	0x07,						// bLength
	0x05,						// bDescriptorType
	BULK_IN_ENDPOINT, 			// bEndpointAddress
	0x02,						// bmAttributes (0x02 = bulk)
	BULK_IN_SIZE, 0x00,			// wMaxPacketSize
	BULK_TRANSMIT_INTERVAL,		// bInterval

	/* Bulk OUT endpoint descriptor. (USB spec 9.6.6, page 269-271, Table 9-13) */
	0x07,						// bLength
	0x05,						// bDescriptorType
	BULK_OUT_ENDPOINT,			// bEndpointAddress
	0x02,						// bmAttributes (0x02 = bulk)
	BULK_OUT_SIZE, 0x00,		// wMaxPacketSize
	BULK_RECEIVE_INTERVAL,		// bInterval

	/* Interface 1 (debug) descriptor. (USB spec 9.6.5, page 267-269, Table 9-12) */
	0x09,						// bLength
	0x04,						// bDescriptorType
	DEBUG_INTERFACE,			// bInterfaceNumber
	0x00,						// bAlternateSetting
	0x01,						// bNumEndpoints
	VENDOR_SPECIFIC,			// bInterfaceClass
	0x01,						// bInterfaceSubClass
	0x01,						// bInterfaceProtocol
	0x00,						// iInterface

	/* Bulk IN endpoint descriptor. (USB spec 9.6.6, page 269-271, Table 9-13) */
	0x07,						// bLength
	0x05,						// bDescriptorType
	DEBUG_IN_ENDPOINT, 			// bEndpointAddress
	0x03,						// bmAttributes (0x03 = interrupt)
	DEBUG_IN_SIZE, 0x00,		// wMaxPacketSize
	DEBUG_TRANSMIT_INTERVAL,	// bInterval

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
	sizeof(CARBON_USB_MANUFACTURER), 0x03, CARBON_USB_MANUFACTURER
};

static const struct usb_string PROGMEM product = {
	sizeof(CARBON_USB_PRODUCT), 0x03, CARBON_USB_PRODUCT
};

const struct descriptor PROGMEM descriptors[] = {
	{ 0x0100, 0x0000, device_descriptor, sizeof(device_descriptor) },
	{ 0x0200, 0x0000, configuration, sizeof(configuration) },
	{ 0x0300, 0x0000, (const uint8_t *)(&language), 4 },
	{ 0x0301, 0x0409, (const uint8_t *)(&manufacturer), sizeof(CARBON_USB_MANUFACTURER) },
	{ 0x0302, 0x0409, (const uint8_t *)(&product), sizeof(CARBON_USB_PRODUCT) },
};
