# Target to build
TARGET := fvm

# Directory where build products are stored.
BUILD := build

# All directories that contain source files
SRC_DIRS := src

# Macro to find all files with the given extension in SRC_DIRS.
find_srcs = $(wildcard *.$1) $(foreach d,$(SRC_DIRS),$(wildcard $d/*.$1))

# All supported source file extensions
SRC_EXTS := c S
# All source files
SRCS := $(foreach ext,$(SRC_EXTS),$(call find_srcs,$(ext)))
# Object files that need to be produced from sources
OBJS := $(patsubst %,$(BUILD)/%.o,$(SRCS))
# Dependency files that are produced during compilation
DEPS := $(OBJS:.o=.d)

INCLUDE_DIRS :=

# All build directories that will be produced
BUILD_DIRS := $(BUILD) $(addprefix $(BUILD)/,$(SRC_DIRS))

# .dir files in every build directory
BUILD_DIR_FILES := $(addsuffix /.dir,$(BUILD_DIRS))

# Aggregate build utilities based on the target platform.
PREFIX =
CC = $(PREFIX)gcc
LD = $(PREFIX)ld
OBJCOPY = $(PREFIX)objcopy
OBJDUMP = $(PREFIX)objdump

# Define compilation and emulation flags.
CFLAGS = -std=c99 -g \
		 -DPLATFORM_HEADER="<flipper/posix/posix.h>"

LDFLAGS = -lflipper

# Print all commands executed when VERBOSE is defined
ifdef VERBOSE
_v :=
else #VERBOSE
_v := @
endif #VERBOSE

# Build the target by default
all: $(TARGET)

# Linking rule
$(TARGET): $(OBJS)
	@echo 'Linking $@'
	$(_v)$(CC) $(CFLAGS) $(LDFLAGS) -o $(BUILD)/$@ $^

install: all
	$(_v)cp $(BUILD)/$(TARGET) /usr/local/bin

uninstall:
	$(_v)rm -r /usr/local/bin/$(TARGET)

run: all
	$(_v)./build/$(TARGET)

# Compiling rule for C sources
$(BUILD)/%.c.o: %.c | $(BUILD_DIR_FILES)
	@echo 'Compiling $<'
	$(_v)$(CC) $(CFLAGS) -I$(<D) -MD -MP -MF $(BUILD)/$*.c.d -c -o $@ $<

# Compiling rule for S sources
$(BUILD)/%.S.o: %.S | $(BUILD_DIR_FILES)
	@echo 'Compiling $<'
	$(_v)$(CC) $(CFLAGS) -I$(<D) -MD -MP -MF $(BUILD)/$*.S.d -c -o $@ $<

# Build dependency rules
-include $(DEPS)

# Make sure that the .dir files aren't automatically deleted after building
.SECONDARY:

%/.dir:
	$(_v)mkdir -p $* && touch $@

clean:
	@echo 'Removing built products'
	$(_v)rm -rf $(BUILD) $(TARGET)

.PHONY: all clean

# Disable stupid built-in rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:
