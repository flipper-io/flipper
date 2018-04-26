# This line *MUST* come before any includes
SELF := $(realpath $(lastword $(MAKEFILE_LIST)))

# Navigate from /usr/local/include/flipper.mk to /usr/local/share/flipper
INCLUDE := $(realpath $(dir $(SELF)))
ASSETS := $(INCLUDE)/../share/flipper

# Directory where build products are stored.
BUILD := build

# Prefix where build projects are installed
PREFIX ?= /usr/local

# List of all target types
TARGETS :=

.PHONY: all clean install uninstall help

all::
install::
uninstall::

help:
	echo "Make"

clean:
	$(_v)rm -rf $(BUILD)

# ---------------------- DEVICE ---------------------- #

DEVICE_TARGET   := $(MODULE)

DEVICE_PREFIX   := arm-none-eabi-

DEVICE_INC_DIRS := include $(INCLUDE)

DEVICE_SRC_DIRS := src

DEVICE_CFLAGS   := -std=c99                    \
                   -Os                         \
                   -g                          \
                   -mthumb                     \
                   -march=armv7e-m             \
                   -mtune=cortex-m4            \
                   -mfloat-abi=soft            \
                   -D__no_err_str__            \
                   -fPIC                       \
                   -DATSAM4S                   \
                   -D__SAM4S16B__              \

DEVICE_LDFLAGS  := -nostartfiles               \
                   -Wl,-T $(ASSETS)/ram.ld     \
                   -Wl,-R $(ASSETS)/osmium.elf \

device: $(DEVICE_TARGET).bin

all:: device

.PHONY: install-device

install-device: $(DEVICE_TARGET).bin
	$(_v)fdfu $(BUILD)/$(DEVICE_TARGET)/$(DEVICE_TARGET).bin

install:: install-atsam4s

# Build the device code
TARGETS += DEVICE

# ---------------------- HOST ---------------------- #

HOST_TARGET   := $(MODULE)_host

HOST_INC_DIRS := include

-include $(BUILD)/gen.mk

HOST_SRC_DIRS := src host $(GEN_DIRS)

$(BUILD)/gen.mk: $(DEVICE_TARGET).elf | $(BUILD)/gen/.dir
	$(_v)fdwarf $(BUILD)/$(DEVICE_TARGET)/$(DEVICE_TARGET).elf c $(BUILD)/gen
	$(_v)echo "GEN_DIRS = $(BUILD)/gen" > $(BUILD)/gen.mk

HOST_CFLAGS   := -std=gnu99             \
                 -g                     \
                 -Wall                  \
                 -Wextra                \
                 -Wno-unused-parameter  \
                 -DPOSIX                \

HOST_LDFLAGS  := -lflipper

fvm: $(HOST_TARGET).so

# Check if this is a module
ifneq ("$(wildcard host)","")
TARGETS += HOST
host: $(HOST_TARGET)
all:: host
endif

# ---------------------- MAKE MAGIC ---------------------- #

ifdef VERBOSE
_v :=
else
_v := @
endif

# Make sure that the .dir files aren't automatically deleted after building.
.SECONDARY:

%/.dir:
	$(_v)mkdir -p $* && touch $@

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
# Generate remaining variables
$1_BUILD := $$(BUILD)/$$($1_TARGET)
$1_ELF :=  $$($1_TARGET).elf
$1_HEX := $$($1_TARGET).hex
$1_BIN := $$($1_TARGET).bin
$1_SO := $$($1_TARGET).so
$1_SRCS += $$(call find_srcs,$$($1_SRC_DIRS))
$1_OBJS := $$(patsubst %,$$($1_BUILD)/%.o,$$($1_SRCS))
$1_DEPS := $$($1_OBJS:.o=.d)
$1_BUILD_DIRS := $$($1_BUILD) $$(addprefix $$($1_BUILD)/,$$(shell find $$($1_SRC_DIRS) -type d))
$1_BUILD_DIR_FILES := $$(addsuffix /.dir,$$($1_BUILD_DIRS))
$1_CFLAGS += $$(foreach inc,$$($1_INC_DIRS),-I$$(inc))
$1_CC := $$($1_PREFIX)gcc
$1_AS := $$($1_PREFIX)gcc
$1_LD := $$($1_PREFIX)gcc
$1_OBJCOPY := $$($1_PREFIX)objcopy
$1_OBJDUMP := $$($1_PREFIX)objdump

# Add target to the all rule
all:: $$($1_TARGET)

.PHONY: $$($1_TARGET)

# Linking rule
$$($1_TARGET): $$($1_OBJS)
	$(_v)$$($1_LD) $$($1_LDFLAGS) -o $$($1_BUILD)/$$@ $$^

# Linking rule
$$($1_ELF): $$($1_OBJS)
	$(_v)$$($1_LD) $$($1_LDFLAGS) -o $$($1_BUILD)/$$@ $$^

# Objcopy-ing rule
$$($1_HEX): $$($1_ELF)
	$(_v)$$($1_OBJCOPY) -O ihex $$($1_BUILD)/$$< $$($1_BUILD)/$$@

# Objcopy-ing rule
$$($1_BIN): $$($1_ELF)
	$(_v)$$($1_OBJCOPY) -O binary $$($1_BUILD)/$$< $$($1_BUILD)/$$@

# Linking rule
$$($1_SO): $$($1_OBJS)
	$(_v)$$($1_LD) -shared -o $$($1_BUILD)/$$@ $$^ $$($1_LDFLAGS)

# Compiling rule for C sources
$$($1_BUILD)/%.c.o: %.c | $$($1_BUILD_DIR_FILES)
	$(_v)$$($1_CC) $$($1_CFLAGS) -I$$(<D) -MD -MP -MF $$($1_BUILD)/$$*.c.d -c -o $$@ $$<

# Compiling rule for S sources
$$($1_BUILD)/%.S.o: %.S | $$($1_BUILD_DIR_FILES)
	$(_v)$$($1_AS) $$($1_ASFLAGS) $$($1_CFLAGS) -I$$(<D) -MD -MP -MF $$($1_BUILD)/$$*.S.d -c -o $$@ $$<

# Build dependency rules
-include $$($1_DEPS)

endef
generate_target = $(eval $(call _generate_target,$1))
#####

# Generate all of the rules for every target
$(foreach target,$(TARGETS),$(call generate_target,$(target)))

# --------------------------------------------------------- #
