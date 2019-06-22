# AVR target variables
COMPILER_PREFIX   := avr-

# Directories that need to be included for this target.
INC_DIRS := carbon/atmegau2/include \
			kernel/include          \
			$(LIB_INC_DIRS)

SRC_DIRS := carbon/atmegau2         \
			kernel/arch/avr8        \
			$(LIB_SRC_DIRS)

GENERATED := $(LIB_GENERATED)

CFLAGS   := -mmcu=atmega32u2        \
			-DARCH=ARCH_AVR8        \
			-D__AVR_ATmega32U2__    \
			-DF_CPU=16000000UL      \
			-DATMEGAU2              \
			-DLF_DISABLE_DEBUG      \
			-DLF_CONFIG_OMIT_ERRORS \
			-Os                     \
			$(LIB_CFLAGS)

LDFLAGS  := -mmcu=atmega32u2        \
			-Wl,--gc-sections

$(call ADD_TARGET,atmegau2)

atmegau2: atmegau2.hex

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
