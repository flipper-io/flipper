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
LDFLAGS :=

$(call ADD_TARGET,qux)

all:: $(BUILD)/qux/qux.so
