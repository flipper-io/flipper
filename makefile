# Directory where build products are stored.
BUILD := build

# Prefix where build projects are installed
PREFIX ?= /usr/local

.PHONY: all install uninstall help

TARGETS :=

all::
install::
uninstall::

help:
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

# Global CFLAGS
GLOBAL_CFLAGS = -std=c99                  \
                -Wall                     \
                -Wextra                   \
                -Wno-unused-parameter     \
                -Wno-expansion-to-defined \
                -Os                       \
                -g                        \

include library/library.mk
include carbon/carbon.mk

PKGCONFIG_DIR := $(PREFIX)/lib/pkgconfig
PKGCONFIG_PATH := $(PKGCONFIG_DIR)/libflipper.pc

define PKGCONFIG_BODY
Name: libflipper
Description: The libflipper C library
Version: 0.0.1
Libs: -L$(PREFIX)/lib -lflipper
Requires.private:
Cflags: -I$(PREFIX)/include -DPOSIX
endef

export PKGCONFIG_BODY

install-pkgconfig:
	$(_v)mkdir -p $(PKGCONFIG_DIR)
	$(_v)rm -f $(PKGCONFIG_PATH)
	$(_v)echo "$$PKGCONFIG_BODY" > $(PKGCONFIG_PATH)

install:: install-pkgconfig

# --- CONSOLE --- #

.PHONY: console

console: libflipper
	$(_v)cargo build --manifest-path=console/Cargo.toml

install-console: console
	$(_v)cargo install --path=console --force

# --- LANGUAGES --- #

.PHONY: language-rust

language-rust: libflipper
	$(_v)cargo build --manifest-path=languages/rust/Cargo.toml

languages:: language-rust

.PHONY: languages

languages:: language-rust

# --- UTILITIES --- #

include utils/utils.mk

# --- TESTS --- #

.PHONY: test

test: libflipper
	$(_v)$(X86_CC) $(GLOBAL_CFLAGS) $(X86_CFLAGS) -Itests/include -o $(BUILD)/test $(call find_srcs, tests/src) -L$(BUILD)/$(X86_TARGET) -lflipper
	$(_v)./$(BUILD)/test

# --- LANGUAGES --- #

install-python:
	$(_v)pip3 install languages/python --upgrade -q

install:: install-python

# -------------------------------------------------------------------- #

# Print all commands executed when VERBOSE is defined
ifdef VERBOSE
_v :=
else #VERBOSE
_v := @
endif #VERBOSE

# Make sure that the .dir files aren't automatically deleted after building.
.SECONDARY:

%/.dir:
	$(_v)mkdir -p $* && touch $@
	@echo ""

# Disable built-in rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:


#####
# find_srcs($1: source directories, $2: source file extensions)
#####
find_srcs = $(foreach sd,$1,$(foreach ext,$(SRC_EXTS),$(shell find $(sd) -name '*.$(ext)')))
#####

# All supported source file extensions.
SRC_EXTS := c S

#####
# generate_target($1: target prefix)
#
# Generate all of the target-specific build rules for the given target.
#####

define _generate_target

# Generate target-specific variables.
$1_BUILD := $$(BUILD)/$$($1_TARGET)
$1_ELF :=  $$($1_TARGET).elf
$1_HEX := $$($1_TARGET).hex
$1_BIN := $$($1_TARGET).bin
$1_A := $$($1_TARGET).a
$1_SO := $$($1_TARGET).so
$1_SRCS += $$(call find_srcs,$$($1_SRC_DIRS))
$1_OBJS := $$(patsubst %,$$($1_BUILD)/%.o,$$($1_SRCS))
$1_DEPS := $$($1_OBJS:.o=.d)
$1_BUILD_DIRS := $$($1_BUILD) $$(addprefix $$($1_BUILD)/,$$(shell find $$($1_SRC_DIRS) -type d))
$1_BUILD_DIR_FILES := $$(addsuffix /.dir,$$($1_BUILD_DIRS))
$1_CFLAGS += $$(foreach inc,$$($1_INC_DIRS),-I$$(inc))
$1_CC := $$($1_PREFIX)gcc
$1_AS := $$($1_PREFIX)gcc
$1_AR := $$($1_PREFIX)ar
$1_LD := $$($1_PREFIX)gcc
$1_OBJCOPY := $$($1_PREFIX)objcopy
$1_OBJDUMP := $$($1_PREFIX)objdump

.PHONY: $$($1_TARGET)

# Rule to make ELF.
$$($1_ELF): $$($1_OBJS)
	$(_v)$$($1_LD) $$($1_LDFLAGS) -o $$($1_BUILD)/$$@ $$^
	@echo ""

# Rule to make HEX.
$$($1_HEX): $$($1_ELF)
	$(_v)$$($1_OBJCOPY) -O ihex $$($1_BUILD)/$$< $$($1_BUILD)/$$@
	@echo ""

# Rule to make BIN.
$$($1_BIN): $$($1_ELF)
	$(_v)$$($1_OBJCOPY) -O binary $$($1_BUILD)/$$< $$($1_BUILD)/$$@
	@echo ""

# Rule to make static library.
$$($1_A): $$($1_OBJS)
	$(_v)$$($1_AR) rcs $$($1_BUILD)/$$@ $$^
	@echo ""

# Rule to make shared library.
$$($1_SO): $$($1_OBJS)
	$(_v)$$($1_LD) -shared -o $$($1_BUILD)/$$@ $$^ $$($1_LDFLAGS)
	@echo ""

# Rule to build C sources.
$$($1_BUILD)/%.c.o: %.c | $$($1_BUILD_DIR_FILES)
	$(_v)$$($1_CC) $(GLOBAL_CFLAGS) $$($1_CFLAGS) -I$$(<D) -MD -MP -MF $$($1_BUILD)/$$*.c.d -c -o $$@ $$<
	@echo ""

# Rule to build preprocessed assembly sources.
$$($1_BUILD)/%.S.o: %.S | $$($1_BUILD_DIR_FILES)
	$(_v)$$($1_AS) $$($1_ASFLAGS) $(GLOBAL_CFLAGS) $$($1_CFLAGS) -I$$(<D) -MD -MP -MF $$($1_BUILD)/$$*.S.d -c -o $$@ $$<
	@echo ""

# Rule to include build dependancies.
-include $$($1_DEPS)

endef

generate_target = $(eval $(call _generate_target,$1))

#####

# Generate all of the rules for every target
$(foreach target,$(TARGETS),$(call generate_target,$(target)))

.PHONY: clean

clean:
	$(_v)rm -rf $(BUILD)
	$(_v)cargo clean --manifest-path=console/Cargo.toml
	$(_v)cargo clean --manifest-path=languages/rust/Cargo.toml
