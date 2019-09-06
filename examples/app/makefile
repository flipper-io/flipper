include flipper.mk

COMPILER_PREFIX   = arm-none-eabi-
INC_DIRS = include
SRC_DIRS = src
CFLAGS = -I/usr/local/include
LDFLAGS = -Wl,-T /usr/local/share/flipper/ram.ld \
           -Wl,-R /usr/local/share/flipper/platforms/atsam4s.elf \

$(eval $(call ADD_TARGET,qux)

all:: $(BUILD)/qux/qux.elf
