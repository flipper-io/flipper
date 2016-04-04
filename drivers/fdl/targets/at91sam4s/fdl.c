#define __private_include__
#include <flipper/fdl.h>
#include <flipper/error.h>
#include <flipper/platform/platform.h>
#include <flipper/fs.h>
#include <flipper/at45.h>
#include <flipper/fs/tree.h>
#include <flipper/usart.h>
#include <flipper/io.h>
#include <flipper/spi.h>

#define LOAD_PAGE 200
#define TOTAL_PAGES 512
#define EFC_KEY 0x5A

#define fdl_base_address(page) (AT91C_IFLASH + (page * AT91C_IFLASH_PAGE_SIZE))


/* ~ FDL ABI Specification ~ */

/*------------------------------+  0x0000     --+
 |        sizeof(driver)        |               |
 +------------------------------+  0x0004       |
 |     sizeof(.data + .bss)     |               |
 +------------------------------+  0x0008       |
 |          GOT offset          |               |- HEADER
 +------------------------------+  0x000c       |
 |         .data offset         |               |
 +------------------------------+  0x0010       |
 |          .bss size           |               |
 +------------------------------+  0x0014     --+
 |            driver            |
 +------------------------------+  0x0010 + [0x0000]
 |            .text             |
 +------------------------------+  0x0010 + [0x0008]
 |             .got             |
 +------------------------------+  0x0010 + [0x000c]
 |            .data             |
 +------------------------------*/

/*
 *
 * When an FDL object is loaded, the Global Offset Table (GOT) must be patched with relocations for the .data and .bss sections.
 * 
 * The contents of the .data section must be copied into RAM at the patched address.
 *
*/


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

	void *base_address;

	/* ~ Obtain the filesystem object for the given key. ~ */
	fsp _leaf = fs_leaf_for_key(_root_leaf, key);

	/* ~ Ensure that we're loading a valid filesystem object. ~ */
	if (!_leaf) {
		error_raise(E_DL_NOT_FOUND, "");
		return NULL;
	}

	/* ~ Dereference the metadata contained by the leaf. ~ */
	leaf *l = at45_dereference(_leaf, sizeof(leaf));

	/* ~ If the loadable has already been loaded, return the address. ~ */
	if (l -> address) {

		base_address = (void *)(l -> address);

		goto cleanup;

	}

	/* ~ Obtain the size of the .data and .bss sections. ~ */
	uint32_t rw_size;
	at45_pull(&rw_size, sizeof(uint32_t), (l -> data + RW_SIZE_OFFSET));

	serprintf("RW size is 0x%08x\n", rw_size);

	/* ~ Allocate memory for the read/write segment. ~ */
	uint32_t *_data = malloc(rw_size);
	if (!_data) {
		error_raise(E_NO_MEM, "");
		goto cleanup;
	}

	serprintf("GOT data is at %p\n", _data);

	uint32_t buncha_data[64];
	at45_pull(&buncha_data[0], sizeof(uint32_t) * 64, l->data);

	serprintf("First 64 words:\n");
	for (int i = 0; i < 64; i++)
	{
		serprintf("%08X ", buncha_data[i]);
		if (i % 8 == 7)
		{
			serprintf("\n");
		}
	}
	serprintf("-------------------------------------\n");

	/* ~ Obtain the address of the global offset table. ~ */
	uint32_t _got_offset;
	at45_pull(&_got_offset, sizeof(uint32_t), (l -> data + GOT_ADDR_OFFSET));
	serprintf("GOT offset is at 0x%08x\n", _got_offset);
	fsp _got = l -> data + _got_offset;

	/* ~ Obtain the base address of the data and bss sections. ~ */
	uint32_t data_base;
	serprintf("data_base = %X\n", data_base);
	at45_pull(&data_base, sizeof(uint32_t), l -> data + DATA_ADDR);

	uint32_t *got_temp = at45_dereference(_got, rw_size);
	serprintf("got_temp table:\n\n");
	for (int i = 0; i < rw_size/sizeof(uint32_t); i++)
	{
		serprintf("[%d] %X\n", i, got_temp[i]);
	}
	serprintf("\n\ngot_temp = %X\n", (uint32_t)got_temp);
	for (int i = 0; i < rw_size/sizeof(uint32_t); i ++) {
		got_temp[i] += _data - data_base;
		serprintf("got_temp[%d] += %X - %X\n  got_temp[%d] == %X\n", i, _data, data_base, i, got_temp[i]);
	}
	at45_push(got_temp, rw_size, _got);
	free(got_temp);

	got_temp = at45_dereference(_got, rw_size);
	for (int i = 0; i < rw_size/sizeof(uint32_t); i ++) {
		serprintf("Got entry %i is 0x%08x\n", i, got_temp[i]);
	}
	free(got_temp);

	serprintf("REWROTE GOT!\n");

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

		/* ~ Write the page into internal flash memory. ~ */
		write_page(LOAD_PAGE + page);

	}

	/* ~ Close the continuous read from external flash. ~ */
	at45_disable();

	/* ~ Calculate the based address of the loaded object. ~ */
	base_address = fdl_base_address(__fdl_brk) + DRIVER_OFFSET;

	/* ~ Rewrite load address in the filesystem. ~ */
	at45_push(&base_address, sizeof(uint32_t), forward(_leaf, leaf, address));

	/* ~ Increment the FDL break value by the number of pages allocated by this loadable. ~ */
	__fdl_brk += total + 1;

	/* ~ Rewrite configuration memory. ~ */
	//	fdl_write_config(__fdl_brk, fdl_config_brk);

cleanup:

	free(l);

	/* ~ Return the address at which the code has been loaded. ~ */
	return base_address;

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
