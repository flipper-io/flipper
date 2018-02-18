# This line *MUST* come before any includes
SELF := $(realpath $(lastword $(MAKEFILE_LIST)))

# Build products directory
BUILD := build

# Navigate from /usr/local/include/flipper.mk to /usr/local/share/flipper
ASSETS := $(realpath $(dir $(SELF))../share/flipper)

# Build settings for code that runs natively on the Flipper device
FLIPPER_BUILD := $(BUILD)/atsam4s
FLIPPER_TARGET := $(FLIPPER_BUILD)/$(MODULE).bin
FLIPPER_SRCS := $(wildcard src/*.c)
FLIPPER_OBJS := $(patsubst %,$(FLIPPER_BUILD)/%.o,$(FLIPPER_SRCS))
FLIPPER_DEPS := $(FLIPPER_OBJS:.o=.d)
FLIPPER_BUILD_DIRS := $(addsuffix /.dir,$(patsubst %/,%,$(sort $(dir $(FLIPPER_OBJS))))) $(FLIPPER_BUILD)/$(FLIPPER_BUILD)/.dir
FLIPPER_CC := arm-none-eabi-gcc
FLIPPER_LD := arm-none-eabi-gcc
FLIPPER_OBJCOPY := arm-none-eabi-objcopy
FLIPPER_OBJDUMP := arm-none-eabi-objdump

ARM_CFLAGS := -std=c99              \
              -Wall                 \
              -Wextra               \
              -Wno-unused-parameter \
              -Os                   \
              -mthumb               \
              -march=armv7e-m       \
              -mtune=cortex-m4      \
              -mfloat-abi=soft      \
              -g                    \
              -ffreestanding        \
              -fPIC                 \
              -I/usr/local/include  \
              -Iinclude             \
              -DATSAM4S

FLIPPER_LDFLAGS := -nostdlib

# Build settings for code that runs on the host and communicates with the Flipper device
HOST_BUILD := $(BUILD)/local
HOST_TARGET := $(HOST_BUILD)/$(MODULE)
HOST_GLUE_TARGET := $(HOST_TARGET).a
HOST_GLUE_SRCS := $(FLIPPER_BUILD)/cbind.c $(HOST_BUILD)/package_data.c
HOST_GLUE_OBJS := $(patsubst %,$(HOST_BUILD)/%.o,$(HOST_GLUE_SRCS))
HOST_SRCS := $(wildcard host/*.c)
HOST_OBJS := $(patsubst %,$(HOST_BUILD)/%.o,$(HOST_SRCS))
HOST_DEPS := $(HOST_GLUE_OBJS:.o=.d) $(HOST_OBJS:.o=.d)
HOST_BUILD_DIRS := $(addsuffix /.dir,$(patsubst %/,%,$(sort $(dir $(HOST_OBJS))))) $(HOST_BUILD)/$(FLIPPER_BUILD)/.dir $(HOST_BUILD)/$(HOST_BUILD)/.dir
HOST_CC := gcc
HOST_LD := gcc
HOST_AR := ar

HOST_CFLAGS := -I. -Iinclude -g -DPOSIX
HOST_LDFLAGS := -g

# Build settings for code that runs on the Flipper Virtual Machine
FVM_BUILD := $(BUILD)/fvm
FVM_TARGET := $(FVM_BUILD)/$(MODULE).so
FVM_SRCS := $(FLIPPER_SRCS) $(FVM_BUILD)/package_data.c
FVM_OBJS := $(patsubst %,$(FVM_BUILD)/%.o,$(FVM_SRCS))
FVM_DEPS := $(FVM_OBJS:.o=.d)
FVM_BUILD_DIRS := $(addsuffix /.dir,$(patsubst %/,%,$(sort $(dir $(FVM_OBJS)))))
FVM_CC := gcc
FVM_LD := gcc

FVM_CFLAGS := -I. -Iinclude -g -DPOSIX
FVM_LDFLAGS := -g

# Build for the Flipper device and host by default, but not FVM
all:: $(FLIPPER_TARGET)

ifeq ($(HOST_SRCS), )
else
all:: $(HOST_TARGET)
endif

# Target when building for the Flipper Virtual Machine
fvm: $(FVM_TARGET)

echo[%]:
    @echo 'Variable "$*" = "$($*)"'

# Flipper build targets
$(FLIPPER_BUILD)/%.c.o: %.c | $(FLIPPER_BUILD_DIRS)
    $(FLIPPER_CC) $(ARM_CFLAGS) -MD -MP -MF $(FLIPPER_BUILD)/$*.c.d -c -o $@ $<

$(FLIPPER_BUILD)/main.elf: $(FLIPPER_OBJS)
    $(FLIPPER_LD) $(FLIPPER_LDFLAGS) -o $@ $^

$(FLIPPER_BUILD)/cbind.c: $(FLIPPER_BUILD)/main.elf
    fdwarf $< $(MODULE) c $(FLIPPER_BUILD)/cbind.c

$(FLIPPER_BUILD)/$(MODULE).elf: $(ASSETS)/ram.ld $(FLIPPER_OBJS) $(FLIPPER_BUILD)/$(FLIPPER_BUILD)/cbind.c.o
    $(FLIPPER_LD) $(FLIPPER_LDFLAGS) -o $@ -Wl,-T$^

$(FLIPPER_TARGET): $(FLIPPER_BUILD)/$(MODULE).elf
    $(FLIPPER_OBJCOPY) --set-section-flags .bss=alloc,load,contents -O binary $< $@

# Keep track of #include dependencies for incremental builds
-include $(FLIPPER_DEPS)

# Host build targets
$(HOST_BUILD)/package_data.c: $(FLIPPER_TARGET) | $(HOST_BUILD_DIRS)
    (cd $(<D) && xxd -i $(<F)) > $@

$(HOST_BUILD)/%.c.o: %.c | $(HOST_BUILD_DIRS)
    $(HOST_CC) $(HOST_CFLAGS) -MD -MP -MF $(HOST_BUILD)/$*.c.d -c -o $@ $<

$(HOST_GLUE_TARGET): $(HOST_GLUE_OBJS)
    $(HOST_AR) -rcs $@ $^

$(HOST_TARGET): $(HOST_BUILD)/$(MODULE).a $(HOST_OBJS)
    $(HOST_LD) $(HOST_LDFLAGS) -lflipper -o $@ $^

# Keep track of #include dependencies for incremental builds
-include $(HOST_DEPS)

# FVM build targets
$(FVM_BUILD)/%.c.o: %.c | $(FVM_BUILD_DIRS)
    $(FVM_CC) $(FVM_CFLAGS) -MD -MP -MF $(FVM_BUILD)/$*.c.d -c -o $@ $<

$(FVM_BUILD)/package_data.c: | $(FVM_BUILD_DIRS)
    echo "unsigned char package_bin[] = {\n};\nunsigned package_bin_len = 0;" > $@

$(FVM_TARGET): $(FVM_OBJS)
    $(FVM_LD) $(FVM_LDFLAGS) -shared -lflipper -o $@ $^

# Keep track of #include dependencies for incremental builds
-include $(FVM_DEPS)

# Display the disassembly of the compiled Flipper module
dump: $(FLIPPER_BUILD)/$(MODULE).elf
    $(FLIPPER_OBJDUMP) -S -z -D $< | less

# Upload the compiled module to the connected Flipper device
install: $(FLIPPER_TARGET)
    fload $<

# Remove built products
clean:
    rm -rf $(BUILD)

# Display usage information
help:
    @echo \
    "Make subcommands:\n" \
    "  all (default) - Build the Flipper module ($(FLIPPER_TARGET)) and the host client ($(HOST_TARGET))\n" \
    "  fvm           - Build the Flipper module for the Flipper Virtual Machine ($(FVM_TARGET))\n" \
    "  dump          - Display the assembly code listing of the built Flipper module\n" \
    "  install       - Build the Flipper module and upload it to a connected Flipper device\n" \
    "  clean         - Remove the entire build directory, containing all build products"

# Make sure that the .dir files aren't automatically deleted after building.
.SECONDARY:

# Automatically create any directory containing a .dir file, for use in target dependencies
%/.dir:
	$(_v)mkdir -p $* && touch $@

# Disable built-in rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

# Targets that don't name files to be created
.PHONY: all fvm dump install clean help
