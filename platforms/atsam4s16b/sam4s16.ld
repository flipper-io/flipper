/* Osmium linker script for the 'Atmel ATSAM4S' microprocessor. */

MEMORY {
	FLASH (rx) : ORIGIN = 0x00400000, LENGTH = 1024K
	SRAM (rw) : ORIGIN = 0x20000000, LENGTH = 128K
}

PROVIDE (__stack_top__ = ORIGIN(SRAM) + LENGTH(SRAM));

SECTIONS {
    . = ORIGIN(FLASH);

    .text : {
        *(.reset .reset.*)
        *(.text .text.*)
        *(.gnu.linkonce.t.*)
        *(.glue_7t)
        *(.glue_7)
        *(.rodata .rodata.*)
        *(.gnu.linkonce.r.*)
    } > FLASH = 0xFF

    . = ALIGN(4);

    ___text_end__ = .;

    PROVIDE(__text_end__ = .);

    .data : AT (___text_end__) {
        __data_start__ = .;

		*(.ramfunc .ramfunc.*)
		*(.data .data.*)
        *(.gnu.linkonce.d*)
        SORT(CONSTRUCTORS)

        . = ALIGN(4);

        __end__ = .;

        PROVIDE (__data_end__ = .);
    } > SRAM

    .bss : {

        __bss_start = .;
        __bss_start__ = .;

        *(.bss .bss.*)
        *(.gnu.linkonce.b*)
        *(COMMON)

        . = ALIGN(4);

        __bss_end__ = .;
    }

    _end = .;

    PROVIDE(end = .);
}
