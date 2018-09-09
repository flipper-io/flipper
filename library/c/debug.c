#include "libflipper.h"

int lf_debug_level = LF_DEBUG_LEVEL_OFF;

void lf_set_debug_level(int level) {
    lf_debug_level = level;
}

void lf_debug_call(struct _fmr_call *call) {
#ifndef LF_DISABLE_DEBUG
    printf("call\n");
    printf("  └─module:    0x%x\n", call->module);
    printf("  └─function:  0x%x\n", call->function);
    char *typestrs[] = { "int8", "int16", "void", "int32", "int", "", "ptr", "int64" };
    printf("  └─return:    %s\n", typestrs[call->ret & 0x7]);
    printf("  └─types:     0x%x\n", call->argt);
    printf("  └─argc:      0x%x (%d arguments)\n", call->argc, call->argc);
    printf("args\n");
    /* Calculate the offset into the packet at which the arguments will be loaded. */
    uint8_t *offset = call->argv;
    lf_types types = call->argt;
    for (lf_argc i = 0; i < call->argc; i ++) {
        lf_type type = types & lf_max_t;
        lf_arg arg = 0;
        memcpy(&arg, offset, lf_sizeof(type));
        printf("  └─ %c%s:   0x%llx\n", ((type & (1 << 3)) ? '\0' : 'u'), typestrs[type & 0x7], arg);
        offset += lf_sizeof(type);
        types >>= 4;
    }
    printf("\n");
#endif
}

void lf_debug_packet(struct _fmr_packet *packet) {
#ifndef LF_DISABLE_DEBUG
    if (lf_debug_level != LF_DEBUG_LEVEL_ALL) return;

    struct _fmr_header hdr = packet->hdr;

    if (hdr.magic == FMR_MAGIC_NUMBER) {
        printf("header\n");
        printf("  └─magic:     0x%x\n", hdr.magic);
        printf("  └─checksum:  0x%x\n", hdr.crc);
        printf("  └─length:    %d bytes (%.02f%%)\n", hdr.len, (float) hdr.len/sizeof(struct _fmr_packet)*100);
        char *classstrs[] = { "exec", "push", "pull", "dyld", "malloc", "free" };
        printf("  └─class:     %s\n", classstrs[hdr.type]);

        struct _fmr_call_packet *invocation = (struct _fmr_call_packet *)(packet);
        struct _fmr_push_pull_packet *pushpull = (struct _fmr_push_pull_packet *)(packet);
        struct _fmr_dyld_packet *dyld = (struct _fmr_dyld_packet *)(packet);
        struct _fmr_memory_packet *mem = (struct _fmr_memory_packet *)(packet);

        switch (hdr.type) {
            case fmr_rpc_class:
                lf_debug_call(&invocation->call);
            break;
            case fmr_push_class:
            case fmr_pull_class:
                printf("length:\n");
                printf("   └─ ptr:     0x%llx\n", pushpull->ptr);
                printf("   └─ len:     0x%x\n\n", pushpull->len);
            break;
            case fmr_dyld_class:
                printf("module:\n");
                printf("   └─ module: '%s'\n", dyld->module);
            break;
            case fmr_malloc_class:
                printf("malloc:\n");
                printf("   └─ size: '0x%x'\n", mem->size);
            break;
            case fmr_free_class:
                printf("free:\n");
                printf("   └─ ptr: '0x%llx'\n", mem->ptr);
            break;
            default:
                printf("invalid packet class.\n");
            break;
        }
        for (size_t i = 1; i <= hdr.len; i ++) {
            printf("0x%02x ", ((uint8_t *)packet)[i - 1]);
            if (i % 8 == 0 && i < hdr.len - 1) printf("\n");
        }
    } else {
        printf("invalid magic number (0x%02x).\n", hdr.magic);
    }
    printf("\n\n-----------\n\n");
#endif
}

void lf_debug_result(struct _fmr_result *result) {
#ifdef __LF_DEBUG__
    if (lf_debug_level != LF_DEBUG_LEVEL_ALL) return;

    printf("response:\n");
    printf("  └─ value:    0x%llx\n", result->value);
    printf("  └─ error:    0x%x\n", result->error);
    printf("\n-----------\n\n");
#endif
}
