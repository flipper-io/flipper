#ifndef fmr_platform_h

#define fmr_platform_h

#include <platform/atmega.h>

enum { _host, _device, _self };

#define fmr_access_array(object) pgm_read_word(&objects[object])

#endif