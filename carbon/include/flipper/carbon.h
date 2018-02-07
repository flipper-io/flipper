#ifndef __carbon_h__
#define __carbon_h__

/* Flipper: Carbon Edition board header file. */
#include <flipper/libflipper.h>

/* Include the header files for all of the standard modules exposed by the toolbox. */
#include <flipper/adc.h>
#include <flipper/button.h>
#include <flipper/dac.h>
#include <flipper/fld.h>
#include <flipper/gpio.h>
#include <flipper/i2c.h>
#include <flipper/is25lp.h>
#include <flipper/led.h>
#include <flipper/pwm.h>
#include <flipper/rtc.h>
#include <flipper/spi.h>
#include <flipper/swd.h>
#include <flipper/task.h>
#include <flipper/temp.h>
#include <flipper/timer.h>
#include <flipper/uart0.h>
#include <flipper/usart.h>
#include <flipper/usb.h>
#include <flipper/wdt.h>

#include <flipper/atsam4s/atsam4s.h>

/* Define the IO numbers for this board. */

enum {
	/* GPIO signals */
	IO_1 = PIO_PA22,
	IO_2 = PIO_PA23,
	IO_3 = PIO_PA2,
	IO_4 = PIO_PA30,
	IO_5 = PIO_PA29,
	IO_6 = PIO_PA28,
	IO_7 = PIO_PA27,
	IO_8 = PIO_PA7,
	IO_9 = PIO_PA8,
	IO_10 = PIO_PA26,
	IO_11 = PIO_PA25,
	IO_12 = PIO_PA24,
	IO_14 = PIO_PA16,
	IO_15 = PIO_PB7,
	IO_16 = PIO_PB6,
	/* USB signals */
	IO_DP = PIO_PB11,
	IO_DM = PIO_PB10,
	/* USART */
	IO_RX = PIO_PA5A_RXD0,
	IO_TX = PIO_PA6A_TXD0,
	/* SPI signals */
	IO_MOSI = PIO_PA13A_MOSI,
	IO_MISO = PIO_PA12A_MISO,
	IO_SCK = PIO_PA14A_SPCK,
	IO_SS = PIO_PA31A_NPCS1,
	/* I2C signals */
	IO_SCL = PIO_PA4A_TWCK0,
	IO_SDA = PIO_PA3A_TWD0,
	/* Analog signals */
	IO_A1 = PIO_PA20X1_AD3,
	IO_A2 = PIO_PA19X1_AD2,
	IO_A3 = PIO_PA18X1_AD1,
	IO_A4 = PIO_PA17X1_AD0,
	IO_A5 = PIO_PB3X1_AD7,
	IO_A6 = PIO_PB2X1_AD6,
	IO_A7 = PIO_PB1X1_AD5,
	IO_A8 = PIO_PB0X1_AD4
};

struct _carbon_context {
	/* Device that handles interacting with the 4s. (ATMEGA16U2) */
	struct _lf_device *_u2;
	/* Microprocessor that handles code execution. (ATSAM4S16B) */
	struct _lf_device *_4s;
};

/* Attaches to all carbon devices. */
int carbon_attach(void);
/* Attaches to a carbon device over the network. */
struct _lf_device *carbon_attach_hostname(char *hostname);

#endif
