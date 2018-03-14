/* RAM loading linker script for Osmium modules and application. */

/* Obtain the entry point address if the main symbol is defined. */
_ENTRY = DEFINED(main) ? ABSOLUTE(main) + 1: 0;
/* Define the entry point. */
ENTRY(_ENTRY);

/* Ensure the module struct is linked in. */
/* ASSERT (!DEFINED(module), "Error: No module structure defined."); */

/* The only memory section we need to worry about is RAM. */
MEMORY {
	RAM (rwx)  : ORIGIN = 0x00000000, LENGTH = 128K
}

SECTIONS {

    /* Place the sections in memory at the beginning of the image. */
	. = ORIGIN(RAM);

    /* Build the Flipper ABI header. */
	.header : {
        /* The address of the entry point (if image is an application). */
        LONG(_ENTRY);
        /* The size of the module structure. */
        LONG(SIZEOF(.module));
        /* The offset into the image at which the module structure exists. */
        LONG(ADDR(.module));
        /* The size of the .data section. */
        LONG(SIZEOF(.data));
        /* The offset into the image at which the .data section exists. */
        LONG(ADDR(.data));
        /* The size of the .bss section. */
        LONG(SIZEOF(.bss));
        /* The offset into the image at which the .bss section exists. */
        LONG(ADDR(.bss));
        /* The size of the .got section. */
        LONG(DEFINED(_GLOBAL_OFFSET_TABLE_) ? SIZEOF(.got) : 0);
        /* The offset into the image at which the .got section exists. */
        LONG(DEFINED(_GLOBAL_OFFSET_TABLE) ? ADDR(.got) : 0);
    } > RAM

	/* The first section placed into memory is the module structure. */
    .module : {
        /* All images start with the entry glue symbols. */
        * (.start);
        /* The module structure. */
        * (.module);
		. = ALIGN(4);
    } > RAM

    /* Next, the text section is placed into memory. */
	.text : {
        * (.text .text.*);
		. = ALIGN(4);
    } > RAM

    /* Followed by the data section. */
	.data : {
        * (.data .data.*);
		. = ALIGN(4);
    } > RAM

    /* Ending with the .bss section, word aligned. */
	.bss : {
        * (.bss .bss.*);
		. = ALIGN(4);
	} > RAM = 0x00

}
