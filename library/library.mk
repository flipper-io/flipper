LIB_INC_DIRS := library/include    \
				library/platforms

LIB_SRC_DIRS := library/src

LIB_CFLAGS   :=

LIB_LDFLAGS  :=

LIBFLIPPER_TARGET := libflipper
LIBFLIPPER_PREFIX :=
LIBFLIPPER_INC_DIRS := $(LIB_INC_DIRS)
LIBFLIPPER_SRC_DIRS := $(LIB_SRC_DIRS) library/arch/x64 library/platforms/posix
LIBFLIPPER_CFLAGS := $(LIB_CFLAGS) $(shell pkg-config --cflags libusb-1.0)
LIBFLIPPER_LDFLAGS := $(LIB_LDFLAGS) $(shell pkg-config --libs libusb-1.0)

TARGETS += LIBFLIPPER

# --- LIBFLIPPER --- #

libflipper: libflipper.so | $(BUILD)/include/flipper/.dir
	$(_v)cp -r carbon/hal/include/* $(BUILD)/include/flipper
	$(_v)cp -r library/include/* $(BUILD)/include/flipper

.PHONY: install-libflipper uninstall-libflipper

install-libflipper: libflipper atsam4s
	$(_v)cp $(BUILD)/libflipper/libflipper.so $(PREFIX)/lib/
	$(_v)cp -r $(BUILD)/include/* $(PREFIX)/include/
	$(_v)cp assets/flipper.mk $(PREFIX)/include/
	$(_v)mkdir -p $(PREFIX)/share/flipper
	$(_v)cp assets/ram.ld $(PREFIX)/share/flipper/
	$(_v)cp $(BUILD)/atsam4s/atsam4s.elf $(PREFIX)/share/flipper/osmium.elf

install:: install-libflipper

uninstall-libflipper:
	$(_v)rm $(PREFIX)/include/flipper.h
	$(_v)rm -r $(PREFIX)/include/flipper
	$(_v)rm $(PREFIX)/lib/libflipper.so
	$(_v)rm -rf $(PREFIX)/share/flipper

uninstall:: uninstall-libflipper
