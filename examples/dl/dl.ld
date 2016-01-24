MEMORY {

	LOADABLE (rwx)  : ORIGIN = 0x00000000, LENGTH = 64K


}

SECTIONS {

    . = ORIGIN(LOADABLE);

	.fdl : { * (.start) * (.fdl) }

    .init : { * (.init) }

    .text : { * (.text .text.*) }

	.data : { * (.data .data.*) * (.got) }

	.bss : { * (.bss) * (COMMON) }

}