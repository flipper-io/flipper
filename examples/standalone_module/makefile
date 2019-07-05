##
## Example Makefile for a Flipper app
##

# This sets the name of the Flipper module being built
MODULE := qux

# This next line *MUST* come after MODULE has been set
include flipper.mk

COMPILER_PREFIX   := arm-none-eabi-

GEN := api.mk
INC_DIRS := include
SRC_DIRS := src
CFLAGS := -I/usr/local/include
LDFLAGS := -Wl,-T /usr/local/share/flipper/ram.ld \
           -Wl,-R /usr/local/share/flipper/atsam4s.elf \

$(call ADD_TARGET,qux)

INC_DIRS := include
SRC_DIRS := host $(BUILD)/qux/gen/api
CFLAGS := -I/usr/local/include -I$(BUILD)/qux/gen/api
LDFLAGS := -L/usr/local/lib -lflipper

$(call ADD_TARGET,qux_host)

all:: $(BUILD)/qux/qux.so $(BUILD)/qux_host/qux_host
