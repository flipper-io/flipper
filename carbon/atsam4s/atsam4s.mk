ATSAM4S_PREFIX   := arm-none-eabi-

# Directories that need to be included for this target.
ATSAM4S_INC_DIRS := kernel/include         \
										carbon/atsam4s/asf/src \
										$(LIB_INC_DIRS)

ATSAM4S_SRC_DIRS := carbon/atsam4s         \
                    kernel/arch/armv7      \
										$(LIB_SRC_DIRS)

ATSAM4S_GENERATED := $(LIB_GENERATED)

ATSAM4S_CFLAGS   := -mcpu=cortex-m4        \
                    -mthumb                \
                    -march=armv7e-m        \
                    -mtune=cortex-m4       \
                    -mfloat-abi=soft       \
                    -DATSAM4S              \
					-D__SAM4S16B__         \
					$(LIB_CFLAGS)

ATSAM4S_LDFLAGS  := -nostartfiles          \
                    -mcpu=cortex-m4        \
                    -mthumb                \
                    -march=armv7e-m        \
                    -mtune=cortex-m4       \
                    -mfloat-abi=soft       \
                    -Wl,--gc-sections      \
					-Wl,-T carbon/atsam4s/config/sam4s16.ld \
					$(LIB_LDFLAGS)


TARGETS += ATSAM4S

atsam4s: ATSAM4S.bin

all:: atsam4s

.PHONY: install-atsam4s

install-atsam4s: utils $(BIN_ATSAM4S_TARGET).bin
	$(_v)mkdir -p $(PREFIX)/share/flipper
	$(_v)cp assets/ram.ld $(PREFIX)/share/flipper
	$(_v)cp $(BUILD)/atsam4s/atsam4s.elf $(PREFIX)/share/flipper

flash-atsam4s:
	$(_v)$(BUILD)/utils/fdfu $(BUILD)/$(BIN_ATSAM4S_TARGET)/$(BIN_ATSAM4S_TARGET).bin

update: install-atmegau2 install-atsam4s
