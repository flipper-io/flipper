BUILD := build

FLIPPER_BUILD := $(BUILD)/flipper
FLIPPER_TARGET := $(FLIPPER_BUILD)/$(MODULE).bin
FLIPPER_SRCS := $(wildcard flipper/*.c) $(FLIPPER_BUILD)/cbind.c
FLIPPER_OBJS := $(patsubst %,$(FLIPPER_BUILD)/%.o,$(FLIPPER_SRCS))
FLIPPER_DEPS := $(FLIPPER_OBJS:.o=.d)
FLIPPER_BUILD_DIRS := $(sort $(dir $(FLIPPER_OBJS)))
FLIPPER_CC := arm-none-eabi-gcc
FLIPPER_LD := arm-none-eabi-gcc
FLIPPER_OBJCOPY := arm-none-eabi-objcopy
FLIPPER_OBJDUMP := arm-none-eabi-objdump

FLIPPER_CFLAGS := \
	-mcpu=cortex-m4 \
	-g \
	-ffreestanding \
	-nostdlib \
	-fPIC \
	-Os \
	-I/usr/local/include \
	-I. \
	-D__ATSAM4S__ \
	-DPLATFORM_HEADER="<flipper/atsam4s/atsam4s.h>"

FLIPPER_LDFLAGS := \
	-mcpu=cortex-m4 \
	-g \
	-ffreestanding \
	-nostdlib \
	-fPIC \
	-Os


HOST_BUILD := $(BUILD)/host
HOST_TARGET := $(HOST_BUILD)/$(MODULE)
HOST_GLUE_TARGET := $(HOST_TARGET).a
HOST_GLUE_SRCS := $(FLIPPER_BUILD)/cbind.c $(HOST_BUILD)/package_data.c
HOST_GLUE_OBJS := $(patsubst %,$(HOST_BUILD)/%.o,$(HOST_GLUE_SRCS))
HOST_SRCS := $(wildcard host/*.c)
HOST_OBJS := $(patsubst %,$(HOST_BUILD)/%.o,$(HOST_SRCS))
HOST_DEPS := $(HOST_GLUE_OBJS:.o=.d) $(HOST_OBJS:.o=.d)
HOST_BUILD_DIRS := $(sort $(dir $(HOST_OBJS)))
HOST_CC := gcc
HOST_LD := gcc
HOST_AR := ar

HOST_CFLAGS := -I. -g
HOST_LDFLAGS := -g

FVM_BUILD := $(BUILD)/fvm
FVM_TARGET := $(FVM_BUILD)/$(MODULE).so
FVM_SRCS := $(FLIPPER_SRCS) $(FVM_BUILD)/package_data.c
FVM_OBJS := $(patsubst %,$(FVM_BUILD)/%.o,$(FVM_SRCS))
FVM_DEPS := $(FVM_OBJS:.o=.d)
FVM_BUILD_DIRS := $(sort $(dir $(FVM_OBJS)))
FVM_CC := gcc
FVM_LD := gcc

FVM_CFLAGS := -I. -g -DPLATFORM_HEADER="<flipper/posix/posix.h>"
FVM_LDFLAGS := -g



all: $(FLIPPER_TARGET) $(HOST_TARGET)


fvm: $(FVM_TARGET)


$(FLIPPER_BUILD)/%.c.o: %.c | $(FLIPPER_BUILD_DIRS)
	$(FLIPPER_CC) -c $(FLIPPER_CFLAGS) -I$(<D) -MD -MP -MF $(FLIPPER_BUILD)/$*.c.d -o $@ $<

$(FLIPPER_BUILD)/main.elf: $(FLIPPER_BUILD)/main.c.o
	$(FLIPPER_LD) $(FLIPPER_LDFLAGS) -o $@ $<

$(FLIPPER_BUILD)/cbind.c: $(FLIPPER_BUILD)/main.elf
	fdwarf $< $(MODULE) c $(FLIPPER_BUILD)/cbind.c

$(FLIPPER_BUILD)/$(MODULE).elf: assets/ram.ld $(FLIPPER_OBJS)
	$(FLIPPER_LD) $(FLIPPER_LDFLAGS) -o $@ -Wl,-T$^

$(FLIPPER_TARGET): $(FLIPPER_BUILD)/$(MODULE).elf
	$(FLIPPER_OBJCOPY) --set-section-flags .bss=alloc,load,contents -O binary $< $@


-include $(FLIPPER_DEPS)


$(HOST_BUILD)/package_data.c: $(FLIPPER_TARGET) | $(HOST_BUILD_DIRS)
	(cd $(HOST_BUILD) && xxd -i $<) > $@

$(HOST_BUILD)/%.c.o: %.c | $(HOST_BUILD_DIRS)
	$(HOST_CC) -c $(HOST_CFLAGS) -I$(<D) -MD -MP -MF $(HOST_BUILD)/$*.c.d -o $@ $<

$(HOST_GLUE_TARGET): $(HOST_GLUE_OBJS)
	$(HOST_AR) -rcs $@ $^

$(HOST_TARGET): $(HOST_BUILD)/$(MODULE).a $(HOST_OBJS)
	$(HOST_LD) $(HOST_LDFLAGS) -lflipper -o $@ $^


-include $(HOST_DEPS)


$(FVM_BUILD)/%.c.o: %.c | $(FVM_BUILD_DIRS)
	$(FVM_CC) -c $(FVM_CFLAGS) -I$(<D) -MD -MP -MF $(FVM_BUILD)/$*.c.d -o $@ $<

$(FVM_BUILD)/package_data.c: | $(FVM_BUILD_DIRS)
	printf "unsigned char package_bin[] = {\n};\nunsigned package_bin_len = 0;\n" > $@

$(FVM_TARGET): $(FVM_OBJS)
	$(FVM_LD) $(FVM_LDFLAGS) -shared -lflipper -o $@ $^


-include $(FVM_DEPS)


dump: $(FLIPPER_BUILD)/$(MODULE).elf
	$(FLIPPER_OBJDUMP) -S -z -D $< | less

install: $(FLIPPER_TARGET)
	fload $<

clean:
	rm -rf $(BUILD)


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

%/.dir:
	$(_v)mkdir -p $* && touch $@

# Disable built-in rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:


.PHONY: all fvm dump install clean help
