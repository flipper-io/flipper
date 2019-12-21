# AVR target variables

CC = avr-gcc
AS = avr-gcc
AR = avr-ar
LD = avr-ld
OBJCOPY = avr-objcopy
OBJDUMP = avr-objdump

GEN = git.mk api.mk

# Directories that need to be included for this target.
INC_DIRS = platforms/atmegau2/include \
			os/include \
			lib

SRC_DIRS = platforms/atmegau2 \
			os/arch/avr8 \
			lib

CFLAGS	= -mmcu=atmega32u2 \
			-Os \
			-gdwarf-2

# WARNING: This is very platform/installation specific. Need to replace this.
LDFLAGS = -mavr35 \
			-Tdata 0x800100 \
			-L/usr/local/Cellar/avr-gcc/9.2.0/avr/lib/avr35 \
			-lc \
			-lm \
			/usr/local/Cellar/avr-gcc/9.2.0/avr/lib/avr35/crtatmega32u2.o \
			-L/usr/local/Cellar/avr-gcc/9.2.0/lib/avr-gcc/9/gcc/avr/9.2.0/avr35 \
			-lgcc \
			--gc-sections

$(eval $(call ADD_TARGET,atmegau2))

atmegau2: $(BUILD)/atmegau2/atmegau2.hex

all:: atmegau2

.PHONY: install-atmegau2

# flashes an image to a flipper in DFU mode
install-atmegau2: atmegau2
	$(_v)dfu-programmer atmega32u2 erase --force
	$(_v)dfu-programmer atmega32u2 flash $(BUILD)/atmegau2/atmegau2.hex
	$(_v)dfu-programmer atmega32u2 start

# boots a flipper in DFU mode
boot:
	$(_v)dfu-programmer atmega32u2 start
