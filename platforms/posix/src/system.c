#define __private_include__
#include <flipper/fmr.h>
#include <platforms/posix.h>

struct _lf_device lf_self = {
	{
		"flipper",
		0xc713,
		LF_VERSION,
		(lf_device_32bit | lf_device_little_endian)
	},
	NULL,
	E_OK,
};
