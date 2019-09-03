GEN = git.mk

CC = /usr/bin/clang
AS = /usr/bin/clang
AR = /usr/bin/ar
LD = /usr/bin/clang
OBJCOPY = /usr/bin/objcopy
OBJDUMP = /usr/bin/objdump

INC_DIRS = . \
			lib \
			platforms/atmegau2/include \
			platforms/atsam4s/include \
			platforms/atsam4s/asf/include \
			platforms $(BUILD)/atsam4s/gen/api

SRC_DIRS = lib \
			os/arch/x64 \
			platforms/posix \
			$(BUILD)/atsam4s/gen/api

ifdef DEBUG
CFLAGS = -fsanitize=address -g -fPIC -DLF_POSIX $(shell pkg-config --cflags libusb-1.0)
LDFLAGS = -fsanitize=address $(shell pkg-config --libs libusb-1.0)
else
CFLAGS = -g -fPIC -DLF_POSIX $(shell pkg-config --cflags libusb-1.0)
LDFLAGS = $(shell pkg-config --libs libusb-1.0)
endif

$(eval $(call ADD_TARGET,libflipper))

# --- LIBFLIPPER --- #

.PHONY: libflipper install-libflipper uninstall-libflipper

libflipper: $(BUILD)/libflipper/libflipper.so | $(BUILD)/include/flipper/.dir $(BUILD)/include/flipper/platforms/atmegau2/.dir $(BUILD)/include/flipper/platforms/atsam4s/.dir $(BUILD)/include/flipper/platforms/posix/.dir
	$(_v)cp -r $(BUILD)/atsam4s/gen/api/*.h $(BUILD)/include/flipper
	$(_v)cp -r lib/*.h $(BUILD)/include/flipper
	$(_v)cp -r lib/*.def $(BUILD)/include/flipper
	$(_v)cp -r lib/carbon/*.h $(BUILD)/include/flipper
	$(_v)cp -r platforms/atmegau2/include/* $(BUILD)/include/flipper/platforms/atmegau2
	$(_v)cp -r platforms/atsam4s/include/* $(BUILD)/include/flipper/platforms/atsam4s
	$(_v)cp -r platforms/atsam4s/asf/include/* $(BUILD)/include/flipper/platforms/atsam4s
	$(_v)cp -r platforms/posix/*.h $(BUILD)/include/flipper/platforms/posix
	$(_v)cp assets/flipper.mk $(BUILD)/include/flipper

all:: libflipper

install-libflipper: libflipper
	$(_v)cp $(BUILD)/libflipper/libflipper.so $(PREFIX)/lib/
	$(_v)mkdir -p $(PREFIX)/include/flipper
	$(_v)cp -r $(BUILD)/include/flipper/* $(PREFIX)/include/flipper
	$(_v)echo "#include <flipper/flipper.h>" > $(PREFIX)/include/flipper.h
	$(_v)echo "#include <flipper/api.h>" >> $(PREFIX)/include/flipper.h
	$(_v)ln -sf $(PREFIX)/include/flipper/flipper.mk $(PREFIX)/include

install:: install-libflipper

uninstall-libflipper:
	$(_v)rm -rf $(PREFIX)/include/flipper
	$(_v)rm -f  $(PREFIX)/include/flipper.h
	$(_v)rm -f  $(PREFIX)/include/flipper.mk
	$(_v)rm -f  $(PREFIX)/lib/libflipper.so
	$(_v)rm -rf $(PREFIX)/share/flipper

uninstall:: uninstall-libflipper
