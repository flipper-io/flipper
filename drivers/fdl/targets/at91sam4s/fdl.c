#define __private_include__
#include <fdl/fdl.h>
#include <error/error.h>
#include <platform.h>

#include <fs/fs.h>
#include <at45/at45.h>
#include <fs/tree.h>
#include <usart/usart.h>
#include <io/io.h>
#include <spi/spi.h>

#define AT45_PAGE_SIZE 528
#define LOAD_PAGE 400
#define TOTAL_PAGES 512
#define EFC_KEY 0x5A

#define fdl_load_address(page) (AT91C_IFLASH + (page * AT91C_IFLASH_PAGE_SIZE))

/* ~ This variable will contain information that relates to the next available page into which content can be loaded. ~ */

uint32_t __fdl_brk;

void fdl_configure(void) {
	
	/* ~ Read the break value in from configuration memory. ~ */
	
	//	fdl_read_config(__fdl_brk, fdl_config_brk);
	
	/* ~ If the break value has been reset, reset it and write it to the configuration. ~ */
	
	//	if (!__fdl_brk) __fdl_brk = LOAD_PAGE;
	
	__fdl_brk = LOAD_PAGE;
	
}

__attribute__((section(".ramfunc"))) void my_delay_ms(unsigned long time) {
	
	for (volatile unsigned int i = 0; (i < (F_CPU / 10250) * (time)); i ++);
	
}

/* ~ The handler function must live in RAM, because the CPU cannot access flash while writing it. ~ */

__attribute__((section(".ramfunc"))) void write_page(uint16_t page) {
	
	set_bits_in_port_with_mask(AT91C_BASE_MC -> MC_FCR, ((EFC_KEY << 24) & AT91C_MC_KEY) | ((page << 8) & AT91C_MC_PAGEN) | AT91C_MC_FCMD_START_PROG);
	
	while (!(AT91C_BASE_MC -> MC_FSR & AT91C_MC_FRDY));
	
	my_delay_ms(1);
	
}

/* ~ Performs dynamic loading. ~ */

extern void (* task_to_execute)(void);

void *fdl_load(uint16_t key) {
	
	void *load_address;
	
	/* ~ Obtain the filesystem object for the given key. ~ */
	
	fsp _leaf = fs_leaf_for_key(_root_leaf, key);
	
	/* ~ Ensure that we're loading a valid filesystem object. ~ */
	
	if (!_leaf) {
		error.raise(E_DL_NOT_FOUND, "");
		return NULL;
	}
	
	/* ~ Dereference the metadata contained by the leaf. ~ */
	
	leaf *l = at45_dereference(_leaf, sizeof(leaf));
	
	/* ~ If the loadable has already been loaded, return the address. ~ */
	
	if (l -> address) {
		error.raise(E_DL_LOADED, "");
		load_address = (void *)(l -> address);
		goto cleanup;
	}
	
	/* ~ Calculate the total number of pages required for the load. ~ */
	
	uint32_t total = ceiling(l -> size, AT91C_IFLASH_PAGE_SIZE);
	
	/* ~ Ensure we have enough available flash to satisfy the request. ~ */
	
	//if (__fdl_brk + total > TOTAL_PAGES) { serprintf("Not enough internal memory to satisfy load request. Have %i and %i was requested.\n", TOTAL_PAGES - __fdl_brk, total); return NULL; }
	
	/* ~ Start the loading process by opening a continuous read from external flash given the page and offset at which the program is located. ~ */
	
	at45_begin_continuous_read((l -> data / AT45_PAGE_SIZE), (l -> data % AT45_PAGE_SIZE));
	
	/* ~ Configure the EFC. 48 clocks per ns. 1 FWS. ~ */
	
	set_bits_in_port_with_mask(AT91C_BASE_MC -> MC_FMR, ((0x30 << 16) & AT91C_MC_FMCN) | AT91C_MC_FWS_1FWS);
	
	/* ~ Enable erase before programming. ~ */
	
	clear_bits_in_port_with_mask(AT91C_BASE_MC -> MC_FMR, AT91C_MC_NEBP);
	
	/* ~ We are now ready to begin bringing in individual bytes of the program from the SPI bus. ~ */
	
	for (uint32_t page = 0; page < total; page ++) {
		
		for (uint8_t word = 0; word < AT91C_IFLASH_PAGE_SIZE / sizeof(uint32_t); word ++) {
			
			/* ~ Load the word from external flash memory. The order is little endian. ~ */
			
			uint32_t value = spi_get(); value |= (spi_get() << 8); value |= (spi_get() << 16); value |= (spi_get() << 24);
			
			/* ~ Write the word into the latch buffer. Writes to the latch buffer must be 32-bit. ~ */
			
			*(uint32_t *)((AT91C_IFLASH + (__fdl_brk * AT91C_IFLASH_PAGE_SIZE)) + (page * AT91C_IFLASH_PAGE_SIZE) + (word * sizeof(uint32_t))) = value;
			
		}
		
		//if (page == 0) *(struct _fdl **)(LOAD_BASE + 4) = &fdl;
		
		write_page(LOAD_PAGE + page);
		
	}
	
	/* ~ Close the continuous read from external flash. ~ */
	
	at45_disable();
	
	/* ~ Calculate the load address of the code. ~ */
	
	load_address = fdl_load_address(__fdl_brk);
	
	/* ~ Rewrite filesystem memory. ~ */
	
	at45_push(&load_address, sizeof(uint32_t), forward(_leaf, leaf, address));
	
	/* ~ Increment the FDL break value by the number of pages allocated by this loadable. ~ */
	
	__fdl_brk += total + 1;
	
	/* ~ Rewrite configuration memory. ~ */
	
	//	fdl_write_config(__fdl_brk, fdl_config_brk);
	
cleanup:
	
	free(l);
	
	//serprintf("Loadable loaded successfully.\n");
	
	/* ~ Return the address at which the code has been loaded. ~ */
	
	return load_address;
	
}

/* ~ Terminates the actively running program. ~ */

void fdl_terminate() {
	
	
	
}

/* ~ Loads a program, and saves it into the OS configuration to be loaded on startup. ~ */

void fdl_launch(uint16_t key) {
	
	/* ~ Resolve a load address for the application and add it to the scheduling system. ~ */
	
	task_to_execute = fdl_load(key);
	
}

void fdl_resolve(uint16_t key, const void *address) {
	
	
	
}
