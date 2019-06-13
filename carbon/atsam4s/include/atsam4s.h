/* Flipper Platform Support Header for the Atmel ATSAM4S */

/* Guard whether or not a target platform has already been defined. */
#ifndef __atsam4s__
#define __atsam4s__

#define __SAM4S16B__
#include "sam4s.h"

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

/* Run at a base frequency of 96MHz. */
#define F_CPU 96000000
/* NOTE: The number of wait states is proportionate to the clock speed defined above. */
#define PLATFORM_WAIT_STATES 5

/* Clock generator settings for a 96MHz master clock. */
#define BOARD_OSCOUNT (CKGR_MOR_MOSCXTST(8))
#define BOARD_PLLBR (CKGR_PLLBR_MULB(60) | CKGR_PLLBR_PLLBCOUNT(1) | CKGR_PLLBR_DIVB(10))
#define BOARD_MCKR (PMC_MCKR_PRES_CLK_1 | PMC_MCKR_CSS_PLLB_CLK)

/* Define interrupt priorities, from highest (0) to lowest (15) priority. */
#define SYSTICK_PRIORITY 0
#define UART0_PRIORITY 1
#define PENDSV_PRIORITY 15

/* Communicate at 1 megabaud. */
#define PLATFORM_BAUDRATE 1000000

#define SAM_FMR_PIN PIO_PA0

#define FLASH_PCS 0
#define FLASH_PCS_PIN PIO_PA11A_NPCS0

#define USER_PCS 1
#define USER_PCS_PIN PIO_PA31A_NPCS1

#endif
