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

#define EFC0_FMR (uint32_t *)(0xFFFFFF60)

#define EFC0_FCR (uint32_t *)(0xFFFFFF64)

#define EFC0_FSR (uint32_t *)(0xFFFFFF68)

#define EFC_KEY					0x5A

#define EFC_FRDY				0x00

#define EFC_NEBP				0x07

#define EFC_FCMD_WP				0x01

#define EFC_FCMD_SLB			0x02

#define EFC_FCMD_WPL			0x03

#define EFC_FCMD_CLB			0x04

#define EFC_FCMD_EA				0x08

#define EFC_FCMD_SGPB			0x0B

#define EFC_FCMD_CGPB			0x0D

#define EFC_FCMD_SSB			0x0F

void fdl_configure(void) {
	
	
	
}

char serbuf[64];

/* ~ The handler function must live in RAM, because the CPU cannot access flash while writing it. ~ */

void write_handler(uint32_t page) __attribute__((section(".ramfunc")));

void write_handler(uint32_t page) {
	
	/* ~ Enable automatic page erasure. ~ */
	
	clear_bit_in_port(EFC_NEBP, *EFC0_FMR);
	
	*EFC0_FCR = (0x5A << 24) | ((LOAD_PAGE + page) << 8) | EFC_FCMD_WP;
	
	while (!(*EFC0_FSR & 1)) {
		
		AT91C_BASE_PIOA -> PIO_PER |= (1 << 7);
		
		AT91C_BASE_PIOA -> PIO_OER |= (1 << 7);
		
		AT91C_BASE_PIOA -> PIO_SODR |= (1 << 7);
		
	}
	
}

__attribute__((section(".ramfunc"))) void fdl_load(uint16_t key) {
	
    fsp _leaf = fs_leaf_for_key(_root_leaf, key);
    
    if (!_leaf) {
        
        /* ~ There is no leaf to match the key provided. ~ */
        
        return;
        
    }
    
	/* ~ Dereference the metadata contained by the leaf. ~ */
	
	leaf *l = at45_dereference(_leaf, sizeof(leaf));
	
	/* ~ Start the loading process by computing the page and offset at which the program is located. ~ */
	
	/* ~ We need to bring the program in from external memory whilst having fine control over the individual bytes being read. ~ */
	
	at45_begin_continuous_read((l -> data / AT45_PAGE_SIZE), (l -> data % AT45_PAGE_SIZE));
	
	/* ~ Configure the EFC. ~ */
	
	*EFC0_FMR = 0x300100;

	/* ~ We are now ready to begin bringing in individual bytes of the program from the SPI bus. ~ */
	
	for (uint32_t page = 0; page < l -> size / AT91C_IFLASH_PAGE_SIZE; page ++) {
		
		for (uint8_t word = 0; word < AT91C_IFLASH_PAGE_SIZE / sizeof(uint32_t); word ++) {
			
			sprintf(serbuf, "Writing word %i of page %i of %i pages total\n", word, page, l -> size / AT91C_IFLASH_PAGE_SIZE);
			
			usart1.push(serbuf, strlen(serbuf));
			
			/* ~ Load the word from external flash memory. The order is little endian. ~ */
			
			uint32_t value = spi_get(); value |= (spi_get() << 8); value |= (spi_get() << 16); value |= (spi_get() << 24);
			
			/* ~ Write the word into the latch buffer. Writes to the latch buffer must be 32-bit. ~ */
			
			*(uint32_t *)(LOAD_BASE + (page * AT91C_IFLASH_PAGE_SIZE) + (word * sizeof(uint32_t))) = value;
			
		}
		
		write_handler(page);
		
		sprintf(serbuf, "Wrote page.\nLock Error: %s, Programming Error: %s, Security Set: %s\n", ((get_bit_from_port(2, *EFC0_FSR)) ? "YES" : "NO"), ((get_bit_from_port(3, *EFC0_FSR)) ? "YES" : "NO"), ((get_bit_from_port(5, *EFC0_FSR)) ? "YES" : "NO"));
		
		usart1.push(serbuf, strlen(serbuf));
		
	}
	
	/* ~ Close the continuous read from external flash. ~ */
	
	at45_disable();
	
	((void (*)(void))(LOAD_BASE))();
	
}

void fdl_resolve(uint16_t key, const void *address) {
	
	
	
}