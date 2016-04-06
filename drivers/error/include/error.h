#ifndef __error_h__
#define __error_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>
#include <flipper/error/strings.h>

enum { E_OK
     , E_FMR_PACKET_CRC
     , E_NO_MEM
     , E_TOO_BIG
     , E_FVM_LOAD
     , E_FVM_SYM
     , E_OPEN_SOCK
     , E_CONN_SOCK
     , E_FLIPPER_UNBOUND
     , E_FLIPPER_NOT_FOUND
     , E_HID_MANAGER
     , E_HID_NO_DEV
     , E_HID_TOO_MANY
     , E_HID_OPEN_DEV
     , E_HID_DISCONN_DEV
     , E_HID_WRITE
     , E_HID_TIMEOUT
     , E_IOKIT_DICT
     , E_DL_NOT_FOUND
     , E_DL_LOAD
     , E_DL_LOADED
     , E_FS_OPEN
     , E_FS_ADD_LEAF
     , E_FS_NO_LEAF
     , E_UNIMPLEMENTED
     , E_GREATEST
     };

/* ~ Expose a defined type for the size of an error code. ~ */
typedef uint16_t uinterror_t;

/* ~ Declare the virtual driver object. ~ */
extern const struct _error {

	void (*configure)(void);
	void (*withold)(void);
	void (*disclose)(void);
	void (*raise)(uinterror_t code, char *format, ...) __attribute__ ((format (printf, 2, 3)));
	uinterror_t (* get)(void);
	void (*clear)(void);

} error;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _error_configure, _error_withold, _error_disclose, _error_raise, _error_clear };

/* ~ Declare all function prototypes for this driver. ~ */
extern void error_configure(void);
extern void error_withold(void);
extern void error_disclose(void);
extern void error_raise(uinterror_t code, char *format, ...) __attribute__ ((format (printf, 2, 3)));
extern uinterror_t error_get(void);
extern void error_clear(void);

extern uint8_t error_disclosed;
extern uinterror_t error_code;

#endif
#endif
