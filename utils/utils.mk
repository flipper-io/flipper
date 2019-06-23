# --- UTILITIES --- #

DEPENDENCIES := libflipper
INC_DIRS := $(BUILD)/include
SRC_DIRS := utils/fdfu/src
LDFLAGS  := -L$(BUILD)/libflipper -lflipper

$(call ADD_TARGET,fdfu)

DEPENDENCIES := libflipper
INC_DIRS := $(BUILD)/include
SRC_DIRS := utils/fdebug/src
CFLAGS   := $(shell pkg-config --cflags libusb-1.0)
LDFLAGS  := -L$(BUILD)/libflipper -lflipper $(shell pkg-config --libs libusb-1.0)

$(call ADD_TARGET,fdebug)

DEPENDENCIES := libflipper
INC_DIRS := $(BUILD)/include
SRC_DIRS := utils/fload/src
LDFLAGS  := -L$(BUILD)/libflipper -lflipper

$(call ADD_TARGET,fload)

DEPENDENCIES := libflipper
INC_DIRS := $(BUILD)/include platforms
SRC_DIRS := utils/fvm/src
LDFLAGS  := -L$(BUILD)/libflipper -lflipper -ldl

$(call ADD_TARGET,fvm)

DEPENDENCIES := libflipper
INC_DIRS := $(BUILD)/include
SRC_DIRS := utils/ftest/src
LDFLAGS  :=  -L$(BUILD)/libflipper -lflipper

$(call ADD_TARGET,ftest)

# --- UTILS --- #

.PHONY: utils install-utils uninstall-utils

utils: $(BUILD)/fdfu/fdfu $(BUILD)/fdebug/fdebug $(BUILD)/fload/fload $(BUILD)/fvm/fvm $(BUILD)/ftest/ftest

all:: utils

install-utils: utils | $(BUILD)/utils/fdwarf/.dir
	$(_v)cp -r $(BUILD)/fdfu/fdfu $(PREFIX)/bin
	$(_v)cp -r $(BUILD)/fdebug/fdebug $(PREFIX)/bin
	$(_v)cp -r $(BUILD)/fvm/fvm $(PREFIX)/bin
	$(_v)cp -r $(BUILD)/ftest/ftest $(PREFIX)/bin
	$(_v)cp utils/fdwarf/fdwarf.py $(PREFIX)/bin
	$(_v)chmod +x $(PREFIX)/bin/fdwarf.py

install:: install-utils

uninstall-utils:
	$(_v)rm $(PREFIX)/bin/fdfu
	$(_v)rm $(PREFIX)/bin/fdebug
	$(_v)rm $(PREFIX)/bin/fload

uninstall:: uninstall-utils
