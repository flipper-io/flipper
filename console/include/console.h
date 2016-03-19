#ifndef __console_h__
#define __console_h__

void wait_with_progress(int seconds);
void progress(int x, int t, int r, int w);
void sam_erase_flash(void);
int sam_load_firmware(char *firmware);

#endif
