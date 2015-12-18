#define __private_include__

#include <fdl/fdl.h>

#include <fs/fs.h>

#include <at45/at45.h>

#include <fs/tree.h>

#include <usart/usart.h>

#include <io/io.h>

#include <spi/spi.h>

#include <platform/at91sam.h>

#define LOAD_PAGE 300

#define LOAD_BASE (AT91C_IFLASH + (LOAD_PAGE * AT91C_IFLASH_PAGE_SIZE))

#define EFC0_FMR ((uint32_t *)(0xFFFFFF60))

#define EFC0_FCR ((uint32_t *)(0xFFFFFF64))

#define EFC0_FSR ((uint32_t *)(0xFFFFFF68))

#define EFC_KEY					0x5A

#define EFC_FRDY				0x00

#define EFC_FCMD_WP				0x01

#define EFC_FCMD_SLB			0x02

#define EFC_FCMD_WPL			0x03

#define EFC_FCMD_CLB			0x04

#define EFC_FCMD_EA				0x08

#define EFC_FCMD_SGPB			0x0B

#define EFC_FCMD_CGPB			0x0D

#define EFC_FCMD_SSB			0x0F

void fdl_configure(void) {
	
	io.direction(7, OUTPUT);
	
	io.write(7, ON);
	
}

void fdl_load(uint16_t key) {
    
    fsp _leaf = fs_leaf_for_key(_root_leaf, key);
    
    if (!_leaf) {
        
        /* ~ There is no leaf to match the key provided. ~ */
        
        return;
        
    }
    
    /* ~ Dereference the metadata contained by the leaf. ~ */
    
    leaf *l = at45_dereference(_leaf, sizeof(leaf));
    
/* --- BEGIN LOADING PROCEDURE --- */
	
	/* ~ Unlock and configure the Flash controller. Sets the number of microseconds for 1 clock of F_CPU. Enables automatic page erasure. ~ */
	
	*EFC0_FMR = 0x340100;
	
	/* ~ Begin a continuous memory array read. ~ */
	
	at45_begin_continuous_read(l -> data / 528, l -> data % 528);
	
	uint32_t pages = l -> size / AT91C_IFLASH_PAGE_SIZE;
	
	for (int i = 0; i < pages; i ++) {
		
		for (int j = 0; j < (AT91C_IFLASH_PAGE_SIZE / sizeof(uint32_t)); j += sizeof(uint32_t)) {
			
			/* ~ Grab a word from external memory. ~ */
			
			uint32_t word = spi_get(); word |= (spi_get() << 8); word |= (spi_get() << 16); word |= (spi_get() << 24);
		
			/* ~ Write the word to the page. ~ */
			
			*(uint32_t *)(LOAD_BASE + (i * AT91C_IFLASH_PAGE_SIZE) + j) = word;
			
		}
		
		/* ~ Write the page into main memory. ~ */
		
		*EFC0_FCR = ((EFC_KEY << 24) | ((LOAD_PAGE + i) << 8) | EFC_FCMD_WP);
		
		/* ~ Wait for the write to be made. ~ */
		
		while (!get_bit_from_port(EFC_FRDY, *EFC0_FSR));
		
		usart1.push((void *)(LOAD_BASE + (i * AT91C_IFLASH_PAGE_SIZE)), AT91C_IFLASH_PAGE_SIZE);
		
	}
	
	at45_disable();
	
	/* ~ Release the memory allocated to dereference the leaf. ~ */
	
	free(l);
	
    /* ~ Configure the ABI. ~ */
	
	//*(struct _fdl **)(LOAD_BASE + 0x4) = (struct _fdl *)(&fdl);
	
    /* ~ Call the initialization routine. ~ */
    
    ((void (*)(void))(LOAD_BASE))();
	
}

void fdl_resolve(uint16_t key, const void *address) {
	
	
	
}