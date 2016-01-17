#define __private_include__

#include <fdl/fdl.h>

#include <fs/fs.h>

#include <at45/at45.h>

#include <fs/tree.h>

#include <usart/usart.h>

#include <io/io.h>

#include <spi/spi.h>

#include <platform/at91sam.h>

#define AT45_PAGE_SIZE 528

#define LOAD_PAGE 400

#define LOAD_BASE (AT91C_IFLASH + (LOAD_PAGE * AT91C_IFLASH_PAGE_SIZE))

#define EFC_KEY 0x5A

void fdl_configure(void) {
	
	
	
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

void fdl_load(uint16_t key) {
	
	/* ~ Obtain the filesystem object for the given key. ~ */
	
    fsp _leaf = fs_leaf_for_key(_root_leaf, key);
	
	/* ~ Ensure that we're loading a valid filesystem object. ~ */
	
    if (!_leaf) return;
    
	/* ~ Dereference the metadata contained by the leaf. ~ */
	
	leaf *l = at45_dereference(_leaf, sizeof(leaf));
	
	/* ~ Start the loading process by opening a continuous read from external flash given the page and offset at which the program is located. ~ */
	
	at45_begin_continuous_read((l -> data / AT45_PAGE_SIZE), (l -> data % AT45_PAGE_SIZE));
	
	/* ~ Configure the EFC. 48 clocks per ns. 1 FWS. ~ */
	
	set_bits_in_port_with_mask(AT91C_BASE_MC -> MC_FMR, ((0x30 << 16) & AT91C_MC_FMCN) | AT91C_MC_FWS_1FWS);
	
	/* ~ Enable erase before programming. ~ */
	
	clear_bits_in_port_with_mask(AT91C_BASE_MC -> MC_FMR, AT91C_MC_NEBP);
	
	/* ~ We are now ready to begin bringing in individual bytes of the program from the SPI bus. ~ */
	
	for (uint32_t page = 0; page < ceiling(l -> size, AT91C_IFLASH_PAGE_SIZE); page ++) {
		
		for (uint8_t word = 0; word < AT91C_IFLASH_PAGE_SIZE / sizeof(uint32_t); word ++) {
			
			/* ~ Load the word from external flash memory. The order is little endian. ~ */
			
			uint32_t value = spi_get(); value |= (spi_get() << 8); value |= (spi_get() << 16); value |= (spi_get() << 24);
			
			/* ~ Write the word into the latch buffer. Writes to the latch buffer must be 32-bit. ~ */
			
			*(uint32_t *)(LOAD_BASE + (page * AT91C_IFLASH_PAGE_SIZE) + (word * sizeof(uint32_t))) = value;
			
		}
		
		if (page == 0) *(struct _fdl **)(LOAD_BASE + 4) = &fdl;
		
		write_page(LOAD_PAGE + page);
		
	}
	
	/* ~ Close the continuous read from external flash. ~ */
	
	at45_disable();
	
	/* ~ CLOSE THE INTERRUPT ~ */
	
	/* ~ Schedule the application launch. ~ */
	
	task_to_execute = LOAD_BASE;
	
	/* ~ Write the loaded program address into the configuration for the startup program. ~ */
	
	at45_push(&task_to_execute, sizeof(uint32_t), config_offset(FDL_CONFIG_BASE, FDL_STARTUP_PROGRAM));
	
}

/* ~ Terminates the actively running program. ~ */

void fdl_terminate() {
	
	
	
}

/* ~ Loads a program, and saves it into the OS configuration to be loaded on startup. ~ */

void fdl_boot(uint16_t key) {
	
	
	
}

void fdl_resolve(uint16_t key, const void *address) {
	
	
	
}