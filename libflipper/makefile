.PHONY: clean

output = libflipper.dylib

target = osx

target_flags = -framework CoreFoundation -framework IOKit

find = $(shell which find)

cc = $(shell which clang)

copy = $(shell which cp)

ln = $(shell which ln)

common = $(shell $(find) ../common -name '*.c')

include_dirs = ../include

includes = $(foreach dir, $(include_dirs), -I "$(dir)")

files = $(shell $(find) . -name platforms -prune -or -name '*.c' -print)

target_files = $(shell $(find) platforms/$(target) -name '*.c' -print)

flags = -std=c99 -undefined dynamic_lookup -dynamiclib -DOVERRIDE_USB

all:

	$(cc) $(flags) $(includes) $(common) $(files) $(target_files) $(target_flags) -o $(output)

	$(copy) $(output) ../sdk/lib

install:

	$(ln) -sf ~/Desktop/flipper-toolbox/sdk/lib/$(output) /usr/local/lib

clean:

	$(find) . -name '*.o' -or -name '$(output)' -exec rm -rf {} \;