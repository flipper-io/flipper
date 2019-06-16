#ifndef __defines_h__
#define __defines_h__

#define FLIPPER_USB_VENDOR_ID 0x16C0
#define FLIPPER_USB_CONTROL_INTERFACE 0

/* Define bit manipulation macros. */
#define lo(x) ((uint8_t)(x))
#define hi(x) ((uint8_t)(x >> 8))
#define lo16(x) ((uint16_t)(((uint32_t)(x))))
#define hi16(x) ((uint16_t)(((uint32_t)(x)) >> 16))
#define little(x) ((((uint16_t)(x)) << 8) | (((uint16_t)(x)) >> 8))
#define little32(x) ((((uint32_t)(x)) << 16) | (((uint32_t)(x)) >> 16))

/* Terminal colors. */
//#define LF_CONFIG_NO_COLOR
#ifdef LF_CONFIG_NO_COLOR
#define KNRM ""
#define KGRN ""
#define KRED ""
#define KBLU ""
#define KYEL ""
#else
#define KNRM "\x1B[0m"
#define KGRN "\x1B[32m"
#define KRED "\x1B[31m"
#define KBLU "\x1B[34m"
#define KYEL "\x1B[33m"
#endif

#define lf_debug(fmt, ...) printf(fmt "\n", ##__VA_ARGS__)

/* Packed attribute. */
#define LF_PACKED __attribute__((__packed__))
/* Weak attribute. */
#define LF_WEAK __attribute__((weak))

#define _LF_XSTR_RAW(x) #x
#define _LF_XSTR(x) _LF_XSTR_RAW(x)

#ifdef __clang__
#define LF_FUNC __attribute__((section("__TEXT,.lm." _LF_XSTR(__FILE_NAME__)), used))
#else
#define LF_FUNC __attribute__((section(SECTION), used))
#endif

/* Used to contain the result of checksumming operations. */
typedef uint16_t lf_crc_t;
/* Used to quantify the version of modules in a standardized format. */
typedef uint16_t lf_version_t;
/* Used to specify return types. */
typedef uint64_t lf_return_t;

#endif /* defined(__defines_h__) */
