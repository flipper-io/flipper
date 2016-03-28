#ifndef __fmr_platform_h__
#define __fmr_platform_h__

enum { _self, _host, _device };

#define fmr_access_array(object) objects[object]

#define host_argument_size 2
#define host_argument(value) little(lo16(value)), little(hi16(value))
#define self_argument_size 2
#define self_argument(value) little(lo16(value)), little(hi16(value))
#define device_argument_size 1
#define device_argument(value) value

#endif
