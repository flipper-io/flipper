# --- UTILITIES --- #

CC = /usr/bin/clang
AS = /usr/bin/clang
LD = /usr/bin/clang

DEPENDENCIES := libflipper
INC_DIRS := $(BUILD)/include
SRC_DIRS := utils/fdfu/src
LDFLAGS  := -L$(BUILD)/libflipper -lflipper

$(call ADD_TARGET,fdfu)

CC = /usr/bin/clang
AS = /usr/bin/clang
LD = /usr/bin/clang

DEPENDENCIES := libflipper
INC_DIRS := $(BUILD)/include
SRC_DIRS := utils/fdebug/src
CFLAGS   := $(shell pkg-config --cflags libusb-1.0)
LDFLAGS  := -L$(BUILD)/libflipper -lflipper $(shell pkg-config --libs libusb-1.0)

$(call ADD_TARGET,fdebug)

CC = /usr/bin/clang
AS = /usr/bin/clang
LD = /usr/bin/clang

DEPENDENCIES := libflipper
INC_DIRS := $(BUILD)/include
SRC_DIRS := utils/fload/src
LDFLAGS  := -L$(BUILD)/libflipper -lflipper

$(call ADD_TARGET,fload)

CC = /usr/bin/clang
AS = /usr/bin/clang
LD = /usr/bin/clang

DEPENDENCIES := libflipper
INC_DIRS := $(BUILD)/include
SRC_DIRS := utils/fvm/src
LDFLAGS  := -L$(BUILD)/libflipper -lflipper -ldl

$(call ADD_TARGET,fvm)

# --- UTILS --- #

.PHONY: utils install-utils uninstall-utils

utils: $(BUILD)/fdfu/fdfu $(BUILD)/fdebug/fdebug $(BUILD)/fload/fload $(BUILD)/fvm/fvm

all:: utils

install-utils: utils | $(BUILD)/utils/fdwarf/.dir
	$(_v)cp -r $(BUILD)/fdfu/fdfu $(PREFIX)/bin
	$(_v)cp -r $(BUILD)/fdebug/fdebug $(PREFIX)/bin
	$(_v)cp -r $(BUILD)/fvm/fvm $(PREFIX)/bin
	$(_v)cp -r $(BUILD)/fload/fload $(PREFIX)/bin
	$(_v)cp utils/fdwarf/fdwarf.py $(PREFIX)/bin/fdwarf
	$(_v)chmod +x $(PREFIX)/bin/fdwarf

install:: install-utils

uninstall-utils:
	$(_v)rm $(PREFIX)/bin/fdfu
	$(_v)rm $(PREFIX)/bin/fdebug
	$(_v)rm $(PREFIX)/bin/fload
	$(_v)rm $(PREFIX)/bin/ftest
	$(_v)rm $(PREFIX)/bin/fvm

uninstall:: uninstall-utils
