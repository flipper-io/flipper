#ifndef __definitions_h__

#define __definitions_h__

#include <flipper/types.h>

typedef uint32_t fp;


#define ON			true

#define on			true

#define OFF			false

#define off			false

#define YES			true

#define yes			true

#define NO			false

#define no			false

#define HIGH		true

#define LOW			false

#define pull_up		true

#define nil			false

#define INPUT		false

#define OUTPUT		true


#define ceiling(x, y) ((x + y - 1) / y)


#define bit(b)                              (0x01 << (b))

#define get_bit_from_port(b, p)             ((p) & bit(b))

#define set_bit_in_port(b, p)               ((p) |= bit(b))

#define set_bits_in_port_with_mask(p, m)	((p) |= (m))

#define clear_bit_in_port(b, p)             ((p) &= ~(bit(b)))

#define clear_bits_in_port_with_mask(p, m)	((p) &= ~(m))

#define flip_bit_in_port(b, p)              ((p) ^= bit(b))

#define flip_bits_in_port_with_mask(p, m)	((p) ^= (m))


#endif