# Target to build
TARGET := libflipper.so

# Directory where build products are stored.
BUILD := build

# All directories that contain source files
SRC_DIRS := carbon/hal \
			library/src \
			library/arch/x86_64 \
			library/platforms/posix \
			runtime/src

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

INCLUDE_DIRS := carbon/include \
				library/include \
				runtime/include

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
CFLAGS = -std=c99 -Os \
		 -DPLATFORM_HEADER="<flipper/posix/posix.h>" \
		 $(foreach inc,$(INCLUDE_DIRS),-I$(inc))

LDFLAGS = -shared -L/usr/local/lib -lusb-1.0

# Print all commands executed when VERBOSE is defined
ifdef VERBOSE
_v :=
else #VERBOSE
_v := @
endif #VERBOSE

# Build the target by default
all: $(TARGET) headers

# Linking rule
$(TARGET): $(OBJS)
	@echo 'Linking $@'
	$(_v)$(CC) $(CFLAGS) $(LDFLAGS) -o $(BUILD)/$@ $^

headers:
	$(_v)cp -r carbon/include/* $(BUILD)/include
	$(_v)cp -r library/include/* $(BUILD)/include
	$(_v)cp -r runtime/include/* $(BUILD)/include
	$(_v)cp flipper.h $(BUILD)/include

install: all
	$(_v)cp $(BUILD)/$(TARGET) /usr/local/lib
	$(_v)cp -r $(BUILD)/include/* /usr/local/include

$(TARGET_HEX): $(TARGET)
	$(_v)$(OBJCOPY) -O ihex $< $@

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
