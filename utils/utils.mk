# --- UTILITIES --- #

.PHONY: utils install-utils uninstall-utils

utils: libflipper | $(BUILD)/utils/.dir
	$(_v)$(LIBFLIPPER_CC) $(GLOBAL_CFLAGS) $(LIB_CFLAGS) -o $(BUILD)/utils/fdfu utils/fdfu/src/*.c -Icarbon/atsam4s/include -Icarbon/asf/include -L$(BUILD)/$(LIBFLIPPER_TARGET) -lflipper
	$(_v)$(LIBFLIPPER_CC) $(GLOBAL_CFLAGS) $(LIB_CFLAGS) -o $(BUILD)/utils/fdebug utils/fdebug/src/*.c $(shell pkg-config --cflags --libs libusb-1.0) -Icarbon/atmegau2/include -Ilibrary/c -L$(BUILD)/$(LIBFLIPPER_TARGET) -lflipper
	$(_v)$(LIBFLIPPER_CC) $(GLOBAL_CFLAGS) $(LIB_CFLAGS) -o $(BUILD)/utils/fload utils/fload/src/*.c -L$(BUILD)/$(LIBFLIPPER_TARGET) -lflipper
	$(_v)$(LIBFLIPPER_CC) $(GLOBAL_CFLAGS) $(LIB_CFLAGS) -o $(BUILD)/utils/fvm $(call find_srcs, utils/fvm/src) -L$(BUILD)/$(LIBFLIPPER_TARGET) -Iplatforms -lflipper -ldl
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
