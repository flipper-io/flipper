include assets/flipper.mk

# Prefix where build products are installed
PREFIX ?= /usr/local

help::
	@echo \
	"\nBuildable targets:\n" \
	"  all (default)       - Build everything\n" \
	"  console             - Build the Flipper console\n" \
	"  libflipper          - Build libflipper\n" \
	"  atmegau2            - Build the co-processor firmware image\n" \
	"  atsam4s             - Build the embedded operating system\n" \
	"\nBuildable languages:\n" \
	"  language-rust       - Build the Rust language bindings\n" \
	"\nInstallable targets:\n" \
	"  install             - Install everything to '$(PREFIX)'\n" \
	"  install-console     - Install the Flipper console to '$(PREFIX)/bin'\n" \
	"  install-libflipper  - Install libflipper to '$(PREFIX)/lib'\n" \
	"  install-atmegau2    - Flash the firmware on the co-processor on the attached device\n" \
	"  install-atsam4s     - Flash the operating system to the attached device\n" \
	"\nInstallable languages:\n" \
	"  install-rust        - Install the Rust language bindings using '$(shell which cargo)'\n" \
	"  install-python      - Install the Python language bindings using '$(shell which pip)'\n" \
	"\nTools:\n" \
	"  update              - Flash the built firmware images to the attached device\n" \
	"  clean               - Remove the entire build directory, containing all built products\n"

include platforms/atmegau2/atmegau2.mk
include platforms/atsam4s/atsam4s.mk
include platforms/posix/posix.mk
include utils/utils.mk

# --- PKGCONFIG --- #

PKGCONFIG_DIR = $(PREFIX)/lib/pkgconfig
PKGCONFIG_PATH = $(PKGCONFIG_DIR)/libflipper.pc

define PKGCONFIG_BODY
Name: libflipper
Description: The libflipper C library
Version: 0.0.1
Libs: -L$(PREFIX)/lib -lflipper
Requires.private:
Cflags: -I$(PREFIX)/include
endef

export PKGCONFIG_BODY

install-pkgconfig:
	$(_v)mkdir -p $(PKGCONFIG_DIR)
	$(_v)rm -f $(PKGCONFIG_PATH)
	$(_v)echo "$$PKGCONFIG_BODY" > $(PKGCONFIG_PATH)

install:: install-pkgconfig

# --- LANGUAGES --- #

install-python:
	$(_v)pip3 install library/python --upgrade -q

# install:: install-python
