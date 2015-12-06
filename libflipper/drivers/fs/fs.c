#define __private_include__

#include <fs/fs.h>

#include <flash/flash.h>

#include <fs/tree.h>

#include <fs/crc.h>

#include <fmr/fmr.h>

void fs_configure(void) {
	
    /* ~ We have to load the freelist and the _break_value in from memory! ~ */
    
    flash_pull(&_free_list, sizeof(fsp), _FREE_LIST);
    
    flash_pull(&_break_value, sizeof(fsp), _BREAK_VALUE);
    
    flash_pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);
	
}

void fs_format(void) {
    
    /* ~ Invoke the formatting operation on the device. ~ */
    
    device.invoke(_fs, _fs_format, NO_ARGS);
    
    fs_configure();
    
}

void fs_print(fsp branch) {
	
	leaf *l = flash_dereference(branch, sizeof(leaf));
	
	printf("Node @ 0x%08x, with name 0x%08x\n L: 0x%08x R: 0x%08x\n", branch, l -> key, l -> left, l -> right);
	
	fs_print(l -> left);
	
	fs_print(l -> right);
	
	free(l);
	
}

void fs_transfer_file(char *path, char *name) {
	
	FILE *file = fopen (path, "r");
	
	fseek(file, 0L, SEEK_END);
	
	uint32_t size = (uint32_t)(ftell(file));
	
	fseek(file, 0L, SEEK_SET);
	
	uint8_t *binary = (uint8_t *) malloc (sizeof(uint8_t) * size);
	
	fread(binary, size, sizeof(uint8_t), file);
	
	uint16_t key = checksum(name, strlen(name));
	
	fsp _leaf = fs_add_leaf_with_key(_root_leaf, key);
	
	/* ~ Allocate space for the file in the filesystem. ~ */
	
	fsp _data = flash_alloc(size);
	
	flash_push(&size, sizeof(fsp), forward(_leaf, leaf, size));
	
	flash_push(&_data, sizeof(fsp), forward(_leaf, leaf, data));
	
    flash_push(binary, size, _data);
	
	free(binary);
	
	fclose(file);
	
}

void fs_download_file(char *name, char *path) {
	
	FILE *file = fopen (path, "w");
	
	/* For simplicity's sake, load the file into memory. The file won't be more than a few kilobytes, which isn't going to hurt anyone. */
	
	uint16_t key = checksum(name, strlen(name));
	
	fsp _leaf = fs_leaf_for_key(_root_leaf, key);
	
	if (!_leaf) {
		
		printf("No file found on the device with name %s.", name);

		return;
		
	}
	
	leaf *l = flash_dereference(_leaf, sizeof(leaf));
	
	if (l -> size > 1000000) return;
	
	uint8_t *data = malloc(l -> size);
    
    for (int i = 0; i < l -> size / 64; i ++) { printf("Receiving %f percent.\n", (float)((float)((i * 64) + (i % 64)) / l -> size) * 100); flash_pull(data + (64 * i), 64, l -> data + (64 * i)); }
    
    flash_push(data, l -> size, l -> data);
	
	fwrite(data, l -> size, sizeof(uint8_t), file);
	
	free(data);
	
	free(l);
	
	fclose(file);
	
}