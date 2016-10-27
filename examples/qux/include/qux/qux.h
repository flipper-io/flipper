#include <flipper.h>

extern struct _qux {
    int (* configure)(void);
    void (* a)(void);
} qux;

extern struct _lf_module _qux;

enum { _qux_configure, _qux_a };

extern int qux_configure(void);
extern void qux_a(void);
