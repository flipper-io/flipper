# ARM target variables

CC = /usr/bin/clang
AS = /usr/bin/clang
AR = arm-none-eabi-ar
LD = arm-none-eabi-ld
OBJCOPY = arm-none-eabi-objcopy
OBJDUMP = arm-none-eabi-objdump

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

CFLAGS   = -mthumb \
		   --target=thumbv7em-unknown-none-elf \
		   -mfloat-abi=soft \
		   -I/usr/local/arm-none-eabi/arm-none-eabi/include \
		   -DATSAM4S \
		   -D__SAM4S16B__ \
		   -gdwarf-2

LDFLAGS  = -L/usr/local/arm-none-eabi/arm-none-eabi/lib/thumb/v7e-m+fp/softfp/ \
		   -lc \
		   -lm \
		   -L/usr/local/arm-none-eabi/lib/gcc/arm-none-eabi/9.1.0/thumb/v7e-m+fp/softfp \
		   -lgcc \
		   -T platforms/atsam4s/config/sam4s16.ld \
		   -no-enum-size-warning


$(call ADD_TARGET,atsam4s)

.PHONY: atsam4s install-atsam4s

atsam4s: $(BUILD)/atsam4s/atsam4s.elf
	$(_v)mkdir -p $(PREFIX)/share/flipper
	$(_v)cp assets/ram.ld $(PREFIX)/share/flipper
	$(_v)cp $(BUILD)/atsam4s/atsam4s.elf $(PREFIX)/share/flipper

all:: atsam4s

install-atsam4s: utils atsam4s $(BUILD)/atsam4s/atsam4s.bin
	$(_v)$(BUILD)/fdfu/fdfu $(BUILD)/atsam4s/atsam4s.bin
