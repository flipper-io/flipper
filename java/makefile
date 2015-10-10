output = libflipperjava

package = flipper

cc = $(shell which 'gcc')

javac = $(shell which 'javac')

javah = $(shell which 'javah')

jar = $(shell which 'jar')

rsync = $(shell which 'rsync')

mkdir = $(shell which 'mkdir')

preprocess = -std=gnu99 -Os -fpic

include_directories = $(JAVA_HOME)/include $(JAVA_HOME)/include/darwin

# Use the 'find' command to isolate all '.java' files for compilation.

javas = $(shell find . -follow -name "*.java")

all: lib install clean processing

lib:

	# Compile the isolated java files.

	$(javac) -cp jna-4.2.0.jar -d package $(javas)

	# Generate the java package as a 'jar' file.

	$(jar) cf $(package).jar -C package $(package)

	# Generate example java executable

	$(javac) -cp flipper.jar main.java

install:

	# Copy the package into the SDK's java directory.

	$(rsync) -r $(package).jar $(FLIPPERSDK)/java/$(package).jar

processing:

	# Create the library directory if it doesn't exist.

	# $(mkdir) -p ~/Documents/Processing/libraries/flipper/library

	# Optionally install the package into Processing's library directory.

	# $(rsync) -r $(FLIPPERSDK)/lib/$(output).dylib $(FLIPPERSDK)/java/$(package).jar library.properties ~/Documents/Processing/libraries/flipper/library

clean:
