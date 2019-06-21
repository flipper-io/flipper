# --- UTILITIES --- #

FDFU_DEPENDENCIES := LIBFLIPPER.so
FDFU_INC_DIRS := $(LIBFLIPPER_INC_DIRS)
FDFU_SRC_DIRS := utils/fdfu/src
FDFU_LDFLAGS  := -L$(BUILD)/libflipper -lflipper
FDFU_GENERATED := $(LIB_GENERATED)

TARGETS += FDFU

FDEBUG_DEPENDENCIES := LIBFLIPPER.so
FDEBUG_INC_DIRS := $(LIBFLIPPER_INC_DIRS)
FDEBUG_SRC_DIRS := utils/fdebug/src
FDEBUG_CFLAGS   := $(shell pkg-config --cflags libusb-1.0)
FDEBUG_LDFLAGS  := -L$(BUILD)/libflipper -lflipper $(shell pkg-config --libs libusb-1.0)
FDEBUG_GENERATED := $(LIB_GENERATED)

TARGETS += FDEBUG

FLOAD_DEPENDENCIES := LIBFLIPPER.so
FLOAD_INC_DIRS := $(LIBFLIPPER_INC_DIRS)
FLOAD_SRC_DIRS := utils/fload/src
FLOAD_LDFLAGS  := -L$(BUILD)/libflipper -lflipper
FLOAD_GENERATED := $(LIB_GENERATED)

TARGETS += FLOAD

FVM_DEPENDENCIES := LIBFLIPPER.so
FVM_INC_DIRS := $(LIBFLIPPER_INC_DIRS)
FVM_SRC_DIRS := utils/fvm/src
FVM_LDFLAGS  := -L$(BUILD)/libflipper -lflipper -ldl
FVM_GENERATED := $(LIB_GENERATED)

TARGETS += FVM

FTEST_DEPENDENCIES := LIBFLIPPER.so
FTEST_INC_DIRS := $(LIBFLIPPER_INC_DIRS)
FTEST_SRC_DIRS := utils/ftest/src
FTEST_LDFLAGS  :=  -L$(BUILD)/libflipper -lflipper
FTEST_GENERATED := $(LIB_GENERATED)

TARGETS += FTEST

# --- UTILS --- #

utils: FDFU.exe FDEBUG.exe FLOAD.exe FVM.exe FTEST.exe | $(BUILD)/utils/fdwarf/.dir
	$(_v)cp utils/fdwarf/fdwarf.py $(BUILD)/utils/fdwarf
	$(_v)chmod +x $(BUILD)/utils/fdwarf

all:: utils

.PHONY: install-utils uninstall-utils

install-utils: utils
	$(_v)cp -r $(BUILD)/utils/* $(PREFIX)/bin

install:: install-utils

uninstall-utils:
	$(_v)rm $(PREFIX)/bin/fdfu
	$(_v)rm $(PREFIX)/bin/fdebug
	$(_v)rm $(PREFIX)/bin/fload

uninstall:: uninstall-utils
