# AVR target variables
COMPILER_PREFIX   := avr-

GEN := git.mk api.mk

# Directories that need to be included for this target.
INC_DIRS := platforms/atmegau2/include \
			os/include \
			lib

SRC_DIRS := platforms/atmegau2 \
			os/arch/avr8 \
			lib

CFLAGS   := -mmcu=atmega32u2 \
			-DARCH=ARCH_AVR8 \
			-D__AVR_ATmega32U2__ \
			-DF_CPU=16000000UL \
			-DATMEGAU2 \
			-DLF_DISABLE_DEBUG \
			-DLF_CONFIG_OMIT_ERRORS \
			-Os

LDFLAGS  := -mmcu=atmega32u2 \
			-Wl,--gc-sections

$(call ADD_TARGET,atmegau2)

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
