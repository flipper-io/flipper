/* loader.h - Primitive type definitions for the Osmium loader. */

#ifndef __loader_h__
#define __loader_h__

/* A value of 1 must be added to all branch addresses to maintain THUMB mode. */
#define THUMB_BIT 1

/* The data structure definition representing the ABI header above. */
struct _fld_header {
    uint32_t entry;
    uint32_t module_size;
    uint32_t module_offset;
    uint32_t data_size;
    uint32_t data_offset;
    uint32_t bss_size;
    uint32_t bss_offset;
    uint32_t got_size;
    uint32_t got_offset;
};

int os_load(void *base);

#endif
