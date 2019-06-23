# Directory where build products are stored.
BUILD := .build

# Prefix where build projects are installed
PREFIX ?= /usr/local

# Print all commands executed when VERBOSE is defined
ifdef VERBOSE
_v :=
else #VERBOSE
_v := @
endif #VERBOSE

.PHONY: all install uninstall help

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
	-g

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

define _ADD_TARGET

$1_CC := $$(COMPILER_PREFIX)gcc
$1_AS := $$(COMPILER_PREFIX)gcc
$1_AR := $$(COMPILER_PREFIX)ar
$1_LD := $$(COMPILER_PREFIX)gcc
$1_OBJCOPY := $$(COMPILER_PREFIX)objcopy
$1_OBJDUMP := $$(COMPILER_PREFIX)objdump

# Generate target-specific variables.
ELF :=  $1.elf
HEX := $1.hex
BIN := $1.bin
EXE := $1
A := $1.a
SO := $1.so

GEN_CFLAGS = -Wno-implicit-function-declaration -DAPIGEN
GEN_LDFLAGS = -Wl,--unresolved-symbols=ignore-all

# Generate!
$(foreach gen,$(GEN),-include $(BUILD)/$1/$(gen))

SRCS += $$(call find_srcs,$$(SRC_DIRS))

GEN_SRCS := $$(foreach d,$$(GEN_DIRS),$$(call find_srcs,$(BUILD)/$1/gen$(d)))

OBJS := $$(patsubst %,$(BUILD)/$1/%,$$(patsubst %.c,%.o,$$(patsubst %.S,%.o,$$(SRCS))))
GEN_OBJS := $$(patsubst %.c,%.o,$$(GEN_SRCS))

BUILD_DIRS := $$(dir $$(OBJS))
BUILD_DIR_FILES := $$(patsubst %,%.dir,$$(BUILD_DIRS))

DEPS := $$(patsubst %.o,$(BUILD)/%.d,$$(OBJS))

$1_ASFLAGS := $$(ASFLAGS)
$1_LDFLAGS := $$(LDFLAGS) $$(GEN_LDFLAGS)
$1_CFLAGS := $$(GEN_CFLAGS) $$(CFLAGS) $$(foreach inc,$$(INC_DIRS),-I$$(inc)) $$(foreach d,$$(GEN_DIRS),-I$(BUILD)/$1/gen$(d))

# Rule to build C sources.
$(BUILD)/$1/%.o: %.S | $$(BUILD_DIR_FILES) $$(DEPENDENCIES)
	$(_v)$$($1_AS) $$($1_ASFLAGS) $(GLOBAL_CFLAGS) $$($1_CFLAGS) -D__FILE_NAME__=$$(basename $$(notdir $$<)) -I$$(<D) -MD -MP -MF $$@.d -c -o $$@ $$<

# Rule to make HEX.
$(BUILD)/$1/$$(HEX): $(BUILD)/$1/$$(ELF)
	$(_v)$$($1_OBJCOPY) -O ihex $$< $$@

# Rule to make BIN.
$(BUILD)/$1/$$(BIN): $(BUILD)/$1/$$(ELF)
	$(_v)$$($1_OBJCOPY) -O binary $$< $$@

# Rule to make executable.
$(BUILD)/$1/$$(EXE): $$(OBJS) $$(GEN_OBJS)
	$(_v)$$($1_LD) -o $$(basename $$@) $$^ $$($1_LDFLAGS)

# Rule to make static library.
$(BUILD)/$1/$$(A): $$(OBJS) $$(GEN_OBJS)
	$(_v)$$($1_AR) rcs $$@ $$^

# Rule to make shared library.
$(BUILD)/$1/$$(SO): $$(OBJS) $$(GEN_OBJS)
	$(_v)$$($1_LD) -shared -o $$@ $$^ $$($1_LDFLAGS)

# Rule to make ELF.
$(BUILD)/$1/gen/$$(ELF): $$(OBJS)
	$(_v)$$($1_LD) $$($1_LDFLAGS) -o $$@ $$^

# Rule to make ELF.
$(BUILD)/$1/$$(ELF): $$(OBJS) $$(GEN_OBJS)
	$(_v)$$($1_LD) $$($1_LDFLAGS) -o $$@ $$^

# Rule to autogenerate the git hash file.
$(BUILD)/$1/git.mk: | $(BUILD)/$1/gen/git/.dir
	$(_v)echo 'const char lf_git_hash[7] = "$$(shell git rev-parse --short HEAD)";' > $(BUILD)/$1/gen/git/git.c
	$(_v)echo 'GEN_DIRS += git' >> $$@
	$(_v)echo 'GEN_CFLAGS =' >> $$@
	$(_v)echo 'GEN_LDFLAGS =' >> $$@

# Rule to build the generated API C files
$(BUILD)/$1/api.mk: $(BUILD)/$1/gen/$$(ELF) | $(BUILD)/$1/gen/api/.dir
	$(_v)python3 utils/fdwarf/fdwarf.py $$< c $(BUILD)/$1/gen/api
	$(_v)echo 'GEN_DIRS += api' >> $$@
	$(_v)echo 'GEN_CFLAGS =' >> $$@
	$(_v)echo 'GEN_LDFLAGS =' >> $$@

# Rule to build C sources.
$(BUILD)/$1/%.o: %.c | $$(BUILD_DIR_FILES) $$(DEPENDENCIES)
	$(_v)$$($1_CC) $(GLOBAL_CFLAGS) $$($1_CFLAGS) -D__FILE_NAME__=$$(basename $$(notdir $$<)) -I$$(<D) -MD -MP -MF $$@.d -c -o $$@ $$<

# Rule to build C sources.
$(BUILD)/$1/gen/%.o: $(BUILD)/$1/gen/%.c
	$(_v)$$($1_CC) $(GLOBAL_CFLAGS) $$($1_CFLAGS) -D__FILE_NAME__=$$(basename $$(notdir $$<)) -I$$(<D) -MD -MP -MF $$@.d -c -o $$@ $$<

# Rule to include build dependancies.
-include $$(DEPS)

undefine COMPILER_PREFIX
undefine INC_DIRS
undefine SRC_DIRS
undefine LDFLAGS
undefine GENERATED
undefine DEPENDENCIES

undefine CC
undefine AS
undefine AR
undefine LD
undefine OBJCOPY
undefine OBJDUMP
undefine ELF
undefine HEX
undefine BIN
undefine EXE
undefine A
undefine SO
undefine GEN
undefine GEN_DIRS
undefine SRCS
undefine GEN_SRCS
undefine OBJS
undefine DEPS
undefine BUILD_DIRS
undefine BUILD_DIR_FILES
undefine CFLAGS

endef

ADD_TARGET = $(eval $(call _ADD_TARGET,$1))

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
Cflags: -I$(PREFIX)/include
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
	$(_v)pip3 install library/python --upgrade -q

# install:: install-python

# -------------------------------------------------------------------- #

# Make sure that the .dir files aren't automatically deleted after building.
.SECONDARY:

%/.dir:
	$(_v)mkdir -p $* && touch $@

# Disable built-in rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

.PHONY: clean

clean:
	$(_v)rm -rf $(BUILD)
