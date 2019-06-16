#ifndef __lf_module_h__
#define __lf_module_h__

struct _lf_module {
    /* A string containing the module's name. */
    char *name;
    /* The version of the module. */
    lf_version_t version;
    /* The loaded index of the module on the device. */
    uint16_t idx;
    /* The module's interface. */
    void **interface;
};

#define LF_MODULE(name) \
    const static struct _lf_module _ ## name ## _module __attribute__((used)) = { _LF_XSTR(name), 0, UINT16_MAX, name ## _interface };

struct _lf_module *lf_module_create(const char *name, uint16_t idx);
void lf_module_release(void *module);

#endif
