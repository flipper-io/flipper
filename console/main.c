#define __private_include__

#include "console.h"

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include <fs/tree.h>

#include <fs/crc.h>

int main(int argc, char *argv[]) {
    
	/* ~ Attatch this instance of libflipper to the first device present over USB. ~ */

	flipper.attach(FLIPPER_SOURCE_USB);

	if (!strcmp(argv[1], "flash")) {

		sam_load_firmware(argv[2]);

	}
    
    else if (!strcmp(argv[1], "load")) {
        
        sam.power(OFF);
        
        fs_format();
        
        /* ~ Get user input. ~ */
        
        char *name = argv[2];
        
        char *path = argv[3];
        
        /* ~ Open the file. ~ */
        
        FILE *file = fopen (path, "r");
        
        if (!file) { printf("\nCould not open the file: %s\n\n", path); exit(EXIT_FAILURE); }
        
        /* ~ Obtain the size of the file. ~ */
        
        fseek(file, 0L, SEEK_END);
        
        uint32_t size = (uint32_t)(ftell(file));
        
        fseek(file, 0L, SEEK_SET);
        
        /* ~ Load the file into memory. ~ */
        
        uint8_t *data = (uint8_t *) malloc (sizeof(uint8_t) * size);
        
        fread(data, size, sizeof(uint8_t), file);
        
        /* ~ Checksum the name to obtain a key. ~ */
        
        uint16_t key = checksum(name, strlen(name));
        
        /* ~ Create an entry in the filesystem. ~ */
        
        fsp _leaf = fs_add_leaf_with_key(_root_leaf, key);
        
        /* ~ Allocate space for the data. ~ */
        
        fsp _data = flash_alloc(size);
        
        /* ~ Move the data into flash. ~ */
        
        for (int i = 0; i < (size / 64); i ++) {
            
            flash_push((void *)(data + (64 * i)), 64, (fsp)(_data + (64 * i)));
            
        }
        
        flash_push((void *)(data + (64 * (size / 64))), (size % 64), (fsp)(_data + (64 * (size / 64))));
        
        /* ~ Rewrite the pointers. ~ */
        
        flash_push(&_data, sizeof(fsp), forward(_leaf, leaf, data));

        flash_push(&size, sizeof(uint32_t), forward(_leaf, leaf, size));
        
        sam.power(ON);
        
        usleep(10000);
        
        /* ~ Load the data. ~ */
        
        fdl.activate(key);
        
        printf("\nLoading successful. Your program is now running.\n\n");
        
    }
	
	else if (!strcmp(argv[1], "format")) {
		
        fs_format();
		
	}
    
    else if (!strcmp(argv[1], "io") && !strcmp("direction", argv[2])) {
        host.invoke(_io, _io_set_direction, 4, little(atoi(argv[3])), 0, little(OUTPUT), 0);
        
    }
    
    else if (!strcmp(argv[1], "io") && !strcmp("write", argv[2])) {
        
        host.invoke(_io, _io_write, 4, little(atoi(argv[3])), 0, little(atoi(argv[4])), 0);
        
    }
    
	return 0;

}
