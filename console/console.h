#ifndef console_h

#define console_h

#include <flipper/flipper.h>

void wait_with_progress(int seconds);

void progress(int x, int t, int r, int w);

void sam_erase_flash(void);

int sam_load_firmware(char *firmware);

#endif
