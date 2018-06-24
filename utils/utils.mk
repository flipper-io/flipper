# --- UTILITIES --- #

.PHONY: utils install-utils uninstall-utils

utils: libflipper | $(BUILD)/utils/.dir
	$(_v)$(LIB_CC) $(GLOBAL_CFLAGS) $(LIB_CFLAGS) -o $(BUILD)/utils/fdfu utils/fdfu/src/*.c -L$(BUILD)/$(X86_TARGET) -lflipper
	$(_v)$(LIB_CC) $(GLOBAL_CFLAGS) $(LIB_CFLAGS) -o $(BUILD)/utils/fdebug utils/fdebug/src/*.c $(shell pkg-config --libs libusb-1.0) -L$(BUILD)/$(X86_TARGET) -lflipper
	$(_v)$(LIB_CC) $(GLOBAL_CFLAGS) $(LIB_CFLAGS) -o $(BUILD)/utils/fload utils/fload/src/*.c -L$(BUILD)/$(X86_TARGET) -lflipper
	$(_v)$(LIB_CC) $(GLOBAL_CFLAGS) $(LIB_CFLAGS) -o $(BUILD)/utils/fvm $(call find_srcs, utils/fvm/src) -L$(BUILD)/$(X86_TARGET) -lflipper -ldl
	$(_v)cp utils/fdwarf/fdwarf.py $(BUILD)/utils/fdwarf
	$(_v)chmod +x $(BUILD)/utils/fdwarf

all:: utils

install-utils: utils
	$(_v)cp -r $(BUILD)/utils/* $(PREFIX)/bin

install:: install-utils

uninstall-utils:
	$(_v)rm $(PREFIX)/bin/fdfu
	$(_v)rm $(PREFIX)/bin/fdebug
	$(_v)rm $(PREFIX)/bin/fload
