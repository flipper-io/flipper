# ARM target variables

CC=arm-none-eabi-gcc
AS=arm-none-eabi-gcc
AR=arm-none-eabi-ar
LD=arm-none-eabi-gcc
OBJCOPY=arm-none-eabi-objcopy
OBJDUMP=arm-none-eabi-objdump

GEN = git.mk api.mk

# Directories that need to be included for this target.
INC_DIRS = os/include \
		   platforms/atsam4s/include \
		   platforms/atsam4s/asf/include \
		   platforms/atsam4s/asf/src \
		   lib

SRC_DIRS = platforms/atsam4s \
		   os/arch/armv7 \
		   lib

CFLAGS   = -mcpu=cortex-m4 \
           -mthumb \
           -march=armv7e-m \
           -mtune=cortex-m4 \
           -mfloat-abi=soft \
           -DATSAM4S \
		   -D__SAM4S16B__

LDFLAGS  = -nostartfiles \
           -mcpu=cortex-m4 \
           -mthumb \
           -march=armv7e-m \
           -mtune=cortex-m4 \
           -mfloat-abi=soft \
           -Wl,--gc-sections \
		   -Wl,-T platforms/atsam4s/config/sam4s16.ld


$(call ADD_TARGET,atsam4s)

.PHONY: atsam4s install-atsam4s

atsam4s: $(BUILD)/atsam4s/atsam4s.elf
	$(_v)mkdir -p $(PREFIX)/share/flipper
	$(_v)cp assets/ram.ld $(PREFIX)/share/flipper
	$(_v)cp $(BUILD)/atsam4s/atsam4s.elf $(PREFIX)/share/flipper

all:: atsam4s

install-atsam4s: utils atsam4s $(BUILD)/atsam4s/atsam4s.bin
	$(_v)$(BUILD)/fdfu/fdfu $(BUILD)/atsam4s/atsam4s.bin
