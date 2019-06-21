LIB_INC_DIRS := library/c \
				api/c \
				carbon/atmegau2/include \
				carbon/atsam4s/include \
				carbon/atsam4s/asf/include \
				carbon/hal/include \

LIB_SRC_DIRS := library/c \
	            api/c

LIB_SRCS := git_hash.c

LIB_CFLAGS   :=
LIB_LDFLAGS  :=

LIBFLIPPER_TARGET := libflipper
LIBFLIPPER_PREFIX :=
LIBFLIPPER_INC_DIRS := $(LIB_INC_DIRS) platforms
LIBFLIPPER_SRC_DIRS := $(LIB_SRC_DIRS) kernel/arch/x64 carbon/hal/src platforms/posix

LIBFLIPPER_SRCS := git_hash.c

ifdef DEBUG
LIBFLIPPER_CFLAGS := $(LIB_CFLAGS) -fsanitize=address -g -fPIC $(shell pkg-config --cflags libusb-1.0)
LIBFLIPPER_LDFLAGS := $(LIB_LDFLAGS) -fsanitize=address $(shell pkg-config --libs libusb-1.0)
else
LIBFLIPPER_CFLAGS := $(LIB_CFLAGS) -g -fPIC $(shell pkg-config --cflags libusb-1.0)
LIBFLIPPER_LDFLAGS := $(LIB_LDFLAGS) $(shell pkg-config --libs libusb-1.0)
endif

TARGETS += LIBFLIPPER

# --- LIBFLIPPER --- #

libflipper: libflipper.so | $(BUILD)/include/flipper/.dir
	$(_v)cp -r api/c/*.h $(BUILD)/include/flipper
	$(_v)cp -r library/c/*.h $(BUILD)/include/flipper
	$(_v)cp -r library/c/*.def $(BUILD)/include/flipper
	$(_v)cp -r carbon/hal/include/*.h $(BUILD)/include/flipper
	$(_v)cp -r carbon/atmegau2/include/* $(BUILD)/include/flipper
	$(_v)cp -r carbon/atsam4s/include/* $(BUILD)/include/flipper
	$(_v)cp -r carbon/atsam4s/asf/include/* $(BUILD)/include/flipper
	$(_v)cp assets/flipper.mk $(BUILD)/include/flipper

all:: libflipper

.PHONY: install-libflipper uninstall-libflipper

install-libflipper: libflipper
	$(_v)cp $(BUILD)/libflipper/libflipper.so $(PREFIX)/lib/
	$(_v)mkdir -p $(PREFIX)/include/flipper
	$(_v)cp -r $(BUILD)/include/flipper/* $(PREFIX)/include/flipper
	$(_v)echo "#include <flipper/flipper.h>" > $(PREFIX)/include/flipper.h
	$(_v)ln -sf $(PREFIX)/include/flipper/flipper.mk $(PREFIX)/include

install:: install-libflipper

uninstall-libflipper:
	$(_v)rm -rf $(PREFIX)/include/flipper
	$(_v)rm -f  $(PREFIX)/include/flipper.h
	$(_v)rm -f  $(PREFIX)/include/flipper.mk
	$(_v)rm -f  $(PREFIX)/lib/libflipper.so
	$(_v)rm -rf $(PREFIX)/share/flipper

uninstall:: uninstall-libflipper
