# ARM target variables

CC = llvm-gcc
AS = llvm-gcc
AR = llvm-ar
LD = lld
OBJCOPY = llvm-objcopy
OBJDUMP = llvm-objdump

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

CFLAGS = -mthumb \
		     --target=thumbv7em-unknown-none-elf \
		     -mfloat-abi=soft \
		     -I/usr/local/Cellar/arm-gcc-bin/9-2019-q4-major/arm-none-eabi/include \
		     -gdwarf-2

LDFLAGS = -L/usr/local/Cellar/arm-gcc-bin/9-2019-q4-major/arm-none-eabi/arm-none-eabi/lib/thumb/v7e-m+fp/softfp/ \
				  -lc \
				  -lm \
				  -L/usr/local/Cellar/arm-gcc-bin/9-2019-q4-major/arm-none-eabi/9.1.0/thumb/v7e-m+fp/softfp \
				  -lgcc \
				  -Tplatforms/atsam4s/config/sam4s16.ld \
				  -no-enum-size-warning


$(eval $(call ADD_TARGET,atsam4s))

.PHONY: atsam4s install-atsam4s

atsam4s: $(BUILD)/atsam4s/atsam4s.elf
	$(_v)mkdir -p $(PREFIX)/share/flipper
	$(_v)cp assets/ram.ld $(PREFIX)/share/flipper
	$(_v)cp $(BUILD)/atsam4s/atsam4s.elf $(PREFIX)/share/flipper

all:: atsam4s

install-atsam4s: utils atsam4s $(BUILD)/atsam4s/atsam4s.bin
	$(_v)$(BUILD)/fdfu/fdfu $(BUILD)/atsam4s/atsam4s.bin
