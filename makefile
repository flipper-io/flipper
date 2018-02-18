# Directory where build products are stored.
BUILD := build

# Prefix where build projects are installed
PREFIX ?= /usr/local

# List of all target types
TARGETS := ARM AVR X86

.PHONY: all install

all::
install::

# ARM target variables
ARM_TARGET     := atsam4s

ARM_PREFIX     := arm-none-eabi-

# Directories that need to be included for this target.
ARM_INC_DIRS := carbon/include        \
                kernel/include        \
                library/include       \
                runtime/include

ARM_SRC_DIRS := carbon/atsam4s        \
                kernel/src            \
                kernel/arch/armv7     \
                runtime/arch/armv7    \
                runtime/src

ARM_CFLAGS     := -std=c99            \
                -Wall                 \
                -Wextra               \
                -Wno-unused-parameter \
                -Os                   \
                -mthumb               \
                -march=armv7e-m       \
                -mtune=cortex-m4      \
                -mfloat-abi=soft      \
                -D__no_err_str__      \
                -DATSAM4S             \
            	$(foreach inc,$(ARM_INC_DIRS),-I$(inc))

ARM_LDFLAGS  := -nostartfiles                    \
                -Wl,-T carbon/atsam4s/sam4s16.ld \
                -Wl,--gc-sections

$(ARM_TARGET): $(ARM_TARGET).bin

.PHONY: install-atsam4s

install-atsam4s: utils $(ARM_TARGET).bin
	$(_v)$(BUILD)/utils/fdfu $(BUILD)/$(ARM_TARGET)/$(ARM_TARGET).bin

# AVR target variables
AVR_TARGET     := atmegau2

AVR_PREFIX     := avr-

# Directories that need to be included for this target.
AVR_INC_DIRS := carbon/include        \
                kernel/include        \
                library/include       \
                runtime/include		  \

AVR_SRC_DIRS := carbon/atmegau2       \
                kernel/src            \
                runtime/arch/avr8     \
                runtime/src

AVR_CFLAGS      := -std=c99           \
                -Wall                 \
                -Wextra               \
                -Wno-unused-parameter \
                -Os                   \
                -mmcu=atmega32u2      \
                -DARCH=ARCH_AVR8      \
                -D__AVR_ATmega32U2__  \
                -DF_CPU=16000000UL    \
                -D__no_err_str__      \
                -DATMEGAU2            \
            	$(foreach inc,$(AVR_INC_DIRS),-I$(inc))

AVR_LDFLAGS  := -mmcu=atmega32u2 \
                -Wl,--gc-sections

$(AVR_TARGET): $(AVR_TARGET).hex

.PHONY: install-atmegau2

install-atmegau2: atmegau2
	$(_v)dfu-programmer atmega32u2 erase
	$(_v)dfu-programmer atmega32u2 flash $(BUILD)/$(AVR_TARGET)/$(AVR_TARGET).hex
	$(_v)dfu-programmer atmega32u2 launch --no-reset

# install:: install-atmegau2

# x86 target variables
X86_TARGET     := libflipper

X86_PREFIX     :=

# Directories that need to be included for this target.
X86_INC_DIRS := carbon/include          \
                library/include         \
                runtime/include

X86_SRC_DIRS := carbon/hal              \
                library/src             \
                runtime/arch/x64        \
                library/platforms/posix \
                runtime/src

X86_CFLAGS :=   -std=gnu99              \
                -Wall                   \
                -Wextra                 \
                -Wno-unused-parameter   \
                -fpic                   \
                -DPOSIX                 \
            	$(foreach inc,$(X86_INC_DIRS),-I$(inc)) \
				$(shell pkg-config --cflags-only-I libusb-1.0)

X86_LDFLAGS  := $(shell pkg-config --libs libusb-1.0)

# --- LIBFLIPPER --- #

libflipper: $(X86_TARGET).so | $(BUILD)/include/flipper/.dir
	$(_v)cp -r carbon/include/flipper/* $(BUILD)/include/flipper
	$(_v)cp -r library/include/flipper/* $(BUILD)/include/flipper
	$(_v)cp -r runtime/include/flipper/* $(BUILD)/include/flipper
	$(_v)cp library/include/flipper.h $(BUILD)/include

.PHONY: install-libflipper uninstall-libflipper

install-libflipper: libflipper
	$(_v)cp $(BUILD)/$(X86_TARGET)/$(X86_TARGET).so $(PREFIX)/lib/
	$(_v)cp -r $(BUILD)/include/* $(PREFIX)/include/
	$(_v)cp assets/flipper.mk $(PREFIX)/include/
	$(_v)mkdir -p $(PREFIX)/share/flipper
	$(_v)cp assets/ram.ld $(PREFIX)/share/flipper/

install:: install-libflipper

uninstall-libflipper:
	$(_v)rm $(PREFIX)/include/flipper.h
	$(_v)rm -r $(PREFIX)/include/flipper
	$(_v)rm $(PREFIX)/lib/$(X86_TARGET).so
	$(_v)rm -rf $(PREFIX)/share/flipper

# --- CONSOLE --- #

.PHONY: console

console: libflipper
	$(_v)cargo build --manifest-path=console/Cargo.toml

install-console: console
	$(_v)cargo install --path=console --force

# --- LANGUAGES --- #

.PHONY: languages

languages:: libflipper

all:: languages

.PHONY: language-rust

language-rust: libflipper
	$(_v)cargo build --manifest-path=languages/rust/Cargo.toml

languages:: language-rust

# --- UTILITIES --- #

.PHONY: utils install-utils uninstall-utils

utils: libflipper | $(BUILD)/utils/.dir
	$(_v)$(X86_CC) $(X86_CFLAGS) -o $(BUILD)/utils/fdfu utils/fdfu/src/*.c -L$(BUILD)/$(X86_TARGET) -lflipper
	$(_v)$(X86_CC) $(X86_CFLAGS) -o $(BUILD)/utils/fdebug utils/fdebug/src/*.c $(shell pkg-config --libs libusb-1.0)
	$(_v)$(X86_CC) $(X86_CFLAGS) -o $(BUILD)/utils/fload utils/fload/src/*.c -L$(BUILD)/$(X86_TARGET) -lflipper
	$(_v)$(X86_CC) $(X86_CFLAGS) -o $(BUILD)/utils/fvm utils/fvm/src/*.c -L$(BUILD)/$(X86_TARGET) -lflipper -ldl
	$(_v)cp utils/fdwarf/fdwarf.py $(BUILD)/utils/fdwarf
	$(_v)chmod +x $(BUILD)/utils/fdwarf

all:: utils

install-utils: utils
	$(_v)cp -r $(BUILD)/utils/* $(PREFIX)/bin

install:: install-utils

uninstall-utils:
	$(_v)rm $(PREFIX)/bin/fdfu
	$(_v)rm $(PREFIX)/bin/fdebug
	$(_v)rm $(PREFIX)/bin/fload

# --- LANGUAGES --- #

PY_DIR = $(shell python -m site --user-site)

install-python:
	$(_v)pip2 install languages/python --upgrade -q

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

# Disable built-in rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:


#####
# find_srcs($1: source directories, $2: source file extensions)
#####
find_srcs = $(foreach e,$2,$(shell find $1 -name "*.$(e)"))
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
$1_SRCS := $$(call find_srcs,$$($1_SRC_DIRS),$$(SRC_EXTS))
$1_OBJS := $$(patsubst %,$$($1_BUILD)/%.o,$$($1_SRCS))
$1_DEPS := $$($1_OBJS:.o=.d)
$1_BUILD_DIRS := $$($1_BUILD) $$(addprefix $$($1_BUILD)/,$$($1_SRC_DIRS))
$1_BUILD_DIR_FILES := $$(addsuffix /.dir,$$($1_BUILD_DIRS))
$1_CC := $$($1_PREFIX)gcc
$1_AS := $$($1_PREFIX)gcc
$1_LD := $$($1_PREFIX)gcc
$1_OBJCOPY := $$($1_PREFIX)objcopy
$1_OBJDUMP := $$($1_PREFIX)objdump

# Add target to the all rule
all:: $$($1_TARGET)

.PHONY: $$($1_TARGET)

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

.PHONY: clean

clean:
	$(_v)rm -rf $(BUILD)
	$(_v)cargo clean --manifest-path=console/Cargo.toml
	$(_v)cargo clean --manifest-path=languages/rust/Cargo.toml
