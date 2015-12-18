#ifndef __sam_h__

#define __sam_h__

#include <flipper/types.h>

extern const struct _sam {
	
	void (* configure)(void);
	
	void (* power)(bool power);
	
	void (* reset)(void);
	
	void (* dfu)(void);
	
	void (* format)(void);
	
	uint32_t (* word)(void *address);
	
} sam;

#ifdef __private_include__

enum { _sam_configure, _sam_set_power, _sam_reset, _sam_load_dfu, _sam_format, _sam_word };

extern void sam_configure(void);

extern void sam_set_power(bool power);

extern void sam_reset(void);

extern void sam_load_dfu(void);

extern void sam_format(void);

extern uint32_t sam_read_word(void *address);

#endif

#endif