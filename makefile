clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf" -or -name "*.bin" -or -name "*.so" -or -name "*.dylib")