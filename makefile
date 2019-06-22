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
EXE := $1.exe
A := $1.a
SO := $1.so

GEN_SRCS := $$(patsubst %,$(BUILD)/$1/gen/%,$$(GENERATED))
GEN_OBJS := $$(patsubst %,%.o,$$(GEN_SRCS))

SRCS += $$(call find_srcs,$$(SRC_DIRS))
OBJS := $$(patsubst %,$(BUILD)/$1/%.o,$$(SRCS)) $$(GEN_OBJS)

DEPS := $$(OBJS:.o=.d)
BUILD_DIRS := $(BUILD)/$1 $$(addprefix $(BUILD)/$1/,$$(shell find $$(SRC_DIRS) -type d))
BUILD_DIR_FILES := $$(addsuffix /.dir,$$(BUILD_DIRS))

$1_ASFLAGS := $$(ASFLAGS)
$1_LDFLAGS := $$(LDFLAGS)
$1_CFLAGS := $$(CFLAGS) $$(foreach inc,$$(INC_DIRS),-I$$(inc))

# Rule to make ELF.
$$(EXE): $$(OBJS) | $$(DEPENDENCIES)
	$(_v)$$($1_LD) -o $$(basename $(BUILD)/$1/$$@) $$^ $$($1_LDFLAGS)

# Rule to make ELF.
$$(ELF): $$(OBJS) | $$(DEPENDENCIES)
	$(_v)$$($1_LD) $$($1_LDFLAGS) -o $(BUILD)/$1/$$@ $$^

# Rule to make HEX.
$$(HEX): $$(ELF)
	$(_v)$$($1_OBJCOPY) -O ihex $(BUILD)/$1/$$< $(BUILD)/$1/$$@

# Rule to make BIN.
$$(BIN): $$(ELF)
	$(_v)$$($1_OBJCOPY) -O binary $(BUILD)/$1/$$< $(BUILD)/$1/$$@

# Rule to make static library.
$$(A): $$(OBJS) | $$(DEPENDENCIES)
	$(_v)$$($1_AR) rcs $(BUILD)/$1/$$@ $$^

# Rule to make shared library.
$$(SO): $$(OBJS) | $$(DEPENDENCIES)
	$(_v)$$($1_LD) -shared -o $(BUILD)/$1/$$@ $$^ $$($1_LDFLAGS)

# Rule to build C sources.
$(BUILD)/$1/%.c.o: %.c | $$(BUILD_DIR_FILES)
	$(_v)$$($1_CC) $(GLOBAL_CFLAGS) $$($1_CFLAGS) -D__FILE_NAME__=$$(basename $$(notdir $$<)) -I$$(<D) -MD -MP -MF $$@.d -c -o $$@ $$<

# Rule to build generated C sources.
$(BUILD)/$1/gen/%.c.o: $$(GEN_SRCS) | $$(BUILD_DIR_FILES)
	$(_v)$$($1_CC) $(GLOBAL_CFLAGS) $$($1_CFLAGS) -D__FILE_NAME__=$$(basename $$(notdir $$<)) -I$$(<D) -MD -MP -MF $$@.d -c -o $$@ $$<

# Rule to build preprocessed assembly sources.
$(BUILD)/$1/%.S.o: %.S | $$(BUILD_DIR_FILES)
	$(_v)$$($1_AS) $$($1_ASFLAGS) $(GLOBAL_CFLAGS) $$($1_CFLAGS) -I$$(<D) -MD -MP -MF $$@.d -c -o $$@ $$<

# Re-generate the git hash every time.
.PHONY: $(BUILD)/$1/gen/git_hash.c

# Rule to autogenerate the git hash file.
$(BUILD)/$1/gen/git_hash.c: | $(BUILD)/$1/gen/.dir
	$(_v)echo 'const char lf_git_hash[7] = "$$(shell git rev-parse --short HEAD)";' > $$@

# Rule to include build dependancies.
-include $$(DEPS)

undefine COMPILER_PREFIX
undefine INC_DIRS
undefine SRC_DIRS
undefine LDFLAGS
undefine GENERATED

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
undefine GEN_SRCS
undefine GEN_OBJS
undefine SRCS
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
