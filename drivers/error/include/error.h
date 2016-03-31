#ifndef __error_h__
#define __error_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>
#include <flipper/error/codes.h>
#include <flipper/error/strings.h>

/* ~ Expose a defined type for the size of an error code. ~ */
typedef uint16_t uinterror_t;

/* ~ Declare the virtual driver object. ~ */
extern const struct _error {

	void (*configure)(void);
	void (*withold)(void);
	void (*disclose)(void);
	void (*raise)(uinterror_t code, char *string);
	void (*clear)(void);
	uint8_t *disclosed;
	uinterror_t *code;

} error;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _error_configure, _error_withold, _error_disclose, _error_raise, _error_clear };

/* ~ Declare all function prototypes for this driver. ~ */
extern void error_configure(void);
extern void error_withold(void);
extern void error_disclose(void);
extern void error_raise(uinterror_t code, char *string);
extern void error_clear(void);

extern uint8_t error_disclosed;
extern uinterror_t error_code;

#endif
#endif
