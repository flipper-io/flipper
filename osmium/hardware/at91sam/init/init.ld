ENTRY(_startup)

MEMORY {

  TEXT (rx)  : ORIGIN = 0x00100000, LENGTH = 0x00010000

  DATA (rw)  : ORIGIN = 0x00200000, LENGTH = 0x00004000

}

PROVIDE (__int_ram_top__ = ORIGIN(DATA) + LENGTH(DATA));

SECTIONS {

    . = ORIGIN(TEXT);

    .text : {

        *(.reset .reset.*)

        *(.text .text.*)

        *(.gnu.linkonce.t.*)

        *(.glue_7t)

        *(.glue_7)

        *(.rodata .rodata.*)

        *(.gnu.linkonce.r.*)


    } > TEXT = 0xFF

    . = ALIGN(4);

    ___text_end__ = .;

    PROVIDE(__text_end__ = .);

    .data : AT (___text_end__) {

        __data_start__ = .;

        *(.data .data.*)

        *(.gnu.linkonce.d*)

        SORT(CONSTRUCTORS)

        . = ALIGN(4);

        __end__ = .;

        PROVIDE (__data_end__ = .);

    } > DATA

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