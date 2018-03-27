/* Flipper Carbon Board Support Header */

#ifndef __carbon_h__
#define __carbon_h__

/* Include the appropriate platform support header. */
#if defined(ATSAM4S)
#include <flipper/atsam4s/atsam4s.h>
#elif defined(ATMEGAU2)
#include <flipper/atmegau2/atmegau2.h>
#elif defined(POSIX)
#include <flipper/posix/posix.h>
#else
#error "Invalid target platform."
#endif

/* Descriptor information for USB. */
#define CARBON_USB_MANUFACTURER    L"flipper.io"
#define CARBON_USB_PRODUCT         L"Flipper: Carbon"
#define CARBON_USB_VENDOR_ID       0x16C0
#define CARBON_USB_PRODUCT_ID      0x0480

/* Define the IO pins for this board. */

/* GPIO signals */
#define IO_1 PIO_PA22
#define IO_2 PIO_PA23
#define IO_3 PIO_PA2
#define IO_4 PIO_PA30
#define IO_5 PIO_PA29
#define IO_6 PIO_PA28
#define IO_7 PIO_PA27
#define IO_8 PIO_PA7
#define IO_9 PIO_PA8
#define IO_10 PIO_PA26
#define IO_11 PIO_PA25
#define IO_12 PIO_PA24
#define IO_14 PIO_PA16
#define IO_15 PIO_PB7
#define IO_16 PIO_PB6
/* USB signals */
#define IO_DP PIO_PB11
#define IO_DM PIO_PB10
/* USART */
#define IO_RX PIO_PA5A_RXD0
#define IO_TX PIO_PA6A_TXD0
/* SPI signals */
#define IO_MOSI PIO_PA13A_MOSI
#define IO_MISO PIO_PA12A_MISO
#define IO_SCK PIO_PA14A_SPCK
#define IO_SS PIO_PA31A_NPCS1
/* I2C signals */
#define IO_SCL PIO_PA4A_TWCK0
#define IO_SDA PIO_PA3A_TWD0
/* Analog signals */
#define IO_A1 PIO_PA20X1_AD3
#define IO_A2 PIO_PA19X1_AD2
#define IO_A3 PIO_PA18X1_AD1
#define IO_A4 PIO_PA17X1_AD0
#define IO_A5 PIO_PB3X1_AD7
#define IO_A6 PIO_PB2X1_AD6
#define IO_A7 PIO_PB1X1_AD5
#define IO_A8 PIO_PB0X1_AD4

/* NOTE: Move these to platform. */
#define DFU_BAUD 0x08
#define FMR_BAUD 0x00

struct _carbon_context {
	/* Device that handles interacting with the u2. (ATMEGA16U2) */
	struct _lf_device *_u2;
	/* Microprocessor that handles code execution. (ATSAM4S16B) */
	struct _lf_device *_4s;
};

/* Attaches to all carbon devices. */
int carbon_attach(void);
/* Attaches to a carbon device over the network. */
struct _lf_device *carbon_attach_hostname(char *hostname);

struct _lf_device *carbon_select_4s(struct _lf_device *device);
struct _lf_device *carbon_select_u2(struct _lf_device *device);

void sam_reset(void);
int sam_enter_dfu(void);
int sam_off(void);
int sam_on(void);

#endif
