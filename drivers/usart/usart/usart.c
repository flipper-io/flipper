#define __private_include__
#include <flipper/usart/usart.h>
#include <flipper/fmr/fmr.h>

/* ------------------------ USART0 ------------------------ */

void usart0_configure(void *baud) {

	device.invoke(_usart, _usart_configure, 1, baud);

}

void usart0_enable(void) {

	device.invoke(_usart, _usart_enable, NO_ARGS);

}

void usart0_disable(void) {

	device.invoke(_usart, _usart_disable, NO_ARGS);

}

bool usart0_ready(void) {

	return (bool)(device.invoke(_usart, _usart_ready, NO_ARGS));

}

void usart0_put(uint8_t byte) {

	device.invoke(_usart, _usart_put, fmr_args(byte));

}

uint8_t usart0_get(void) {

	return (uint8_t)(device.invoke(_usart, _usart_get, NO_ARGS));

}

void usart0_push(void *source, uint32_t length) {

	device.push(_usart, _usart_push, NO_ARGS, source, length);

}

void usart0_pull(void *destination, uint32_t length) {

	device.pull(_usart, _usart_pull, NO_ARGS, destination, length);

}

/* ------------------------ USART1 ------------------------ */

void usart1_configure(void *baud) {

	host.invoke(_usart1, _usart_configure, 1, baud);

}

void usart1_enable(void) {

	host.invoke(_usart1, _usart_enable, NO_ARGS);

}

void usart1_disable(void) {

	host.invoke(_usart1, _usart_disable, NO_ARGS);

}

bool usart1_ready(void) {

	return (bool)(host.invoke(_usart1, _usart_ready, NO_ARGS));

}

void usart1_put(uint8_t byte) {

	host.invoke(_usart1, _usart_put, fmr_args(byte));

}

uint8_t usart1_get(void) {

	return (uint8_t)(host.invoke(_usart1, _usart_get, NO_ARGS));

}

void usart1_push(void *source, uint32_t length) {

	host.push(_usart1, _usart_push, NO_ARGS, source, length);

}

void usart1_pull(void *destination, uint32_t length) {

	host.pull(_usart1, _usart_pull, NO_ARGS, destination, length);

}

/* ------------------------ DBGU ------------------------ */

void dbgu_configure(void *baud) {

	host.invoke(_dbgu, _usart_configure, 1, baud);

}

void dbgu_enable(void) {

	host.invoke(_dbgu, _usart_enable, NO_ARGS);

}

void dbgu_disable(void) {

	host.invoke(_dbgu, _usart_disable, NO_ARGS);

}

bool dbgu_ready(void) {

	return (bool)(host.invoke(_dbgu, _usart_ready, NO_ARGS));

}

void dbgu_put(uint8_t byte) {

	host.invoke(_dbgu, _usart_put, NO_ARGS);

}

uint8_t dbgu_get(void) {

	return (uint8_t)(host.invoke(_dbgu, _usart_get, NO_ARGS));

}

void dbgu_push(void *source, uint32_t length) {

	host.push(_dbgu, _usart_push, NO_ARGS, source, length);

}

void dbgu_pull(void *destination, uint32_t length) {

	host.pull(_dbgu, _usart_pull, NO_ARGS, destination, length);

}
