#ifndef __fmr_platform_h__
#define __fmr_platform_h__

#include "standards.h"

enum { _host, _device, _self };

#define fmr_access_array(object) pgm_read_word(&objects[object])

#endif