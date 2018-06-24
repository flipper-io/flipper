# AVR target variables
ATMEGAU2_TARGET   := atmegau2

ATMEGAU2_PREFIX   := avr-

# Directories that need to be included for this target.
ATMEGAU2_INC_DIRS := carbon/atmegau2/include \
                     kernel/include          \
                     library/include         \

ATMEGAU2_SRC_DIRS := carbon/atmegau2         \
                     library/arch/avr8       \
                     library/src

ATMEGAU2_CFLAGS   := -mmcu=atmega32u2        \
                     -DARCH=ARCH_AVR8        \
                     -D__AVR_ATmega32U2__    \
                     -DF_CPU=16000000UL      \
                     -DATMEGAU2              \

ATMEGAU2_LDFLAGS  := -mmcu=atmega32u2 \
				     -Wl,--gc-sections

TARGETS += ATMEGAU2

atmegau2: atmegau2.hex

.PHONY: install-atmegau2

# flashes an image to a flipper in DFU mode
install-atmegau2: atmegau2
	$(_v)dfu-programmer atmega32u2 erase --force
	$(_v)dfu-programmer atmega32u2 flash $(BUILD)/$(ATMEGAU2_TARGET)/$(ATMEGAU2_TARGET).hex
	$(_v)dfu-programmer atmega32u2 start

# boots a flipper in DFU mode
boot:
	$(_v)dfu-programmer atmega32u2 start
