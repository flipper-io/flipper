#ifndef __fmr_platform_h__
#define __fmr_platform_h__

#include "standards.h"

enum { _host, _device, _self };

#define fmr_access_array(object) pgm_read_word(&objects[object])

#define host_argument_size 2
#define host_argument(value) little(lo16(value)), little(hi16(value))
#define self_argument_size 1
#define self_argument(value) value
#define device_argument_size 2
#define device_argument(value) little(lo16(value)), little(hi16(value))
#define arg32(value) hi16(value), lo16(value)

#endif