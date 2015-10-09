/*---------------------------------------------------------------------------*/
/*-         ATMEL Microcontroller Software Support  -  ROUSSET  -            */
/*---------------------------------------------------------------------------*/
/* The software is delivered "AS IS" without warranty or condition of any    */
/* kind, either express, implied or statutory. This includes without         */
/* limitation any warranty or condition with respect to merchantability or   */
/* fitness for any particular purpose, or against the infringements of       */
/* intellectual property rights of others.                                   */
/*---------------------------------------------------------------------------*/
/*- File source          : GCC_FLASH.ld                                      */
/*- Object               : Linker Script File for Flash Workspace            */
/*- Compilation flag     : None                                              */
/*-                                                                          */
/*- 1.0 20/Oct/04 JPP    : Creation                                          */
/*---------------------------------------------------------------------------*/

/* slightly modified for the WinARM example - M.Thomas (not Atmel) */
/* mthomas 9/2007 update for -ffunction-section -fdata-section     */
/* mthomas 9/2007 replaced stack memory section by provide         */

/* ENTRY important for -ffunction-sections and
assembler-functions (.func) in startup
so the linker finds the "start of the call chain" */
ENTRY(_startup)

/* Memory Definitions */
/* some flash-programmers expect hex files with start-address
0x0 others 0x00100000. Flash is mapped to 0x0 at startup
*/
MEMORY
{
CODE (rx)  : ORIGIN = 0x00100000, LENGTH = 0x00010000
/*  CODE (rx)  : ORIGIN = 0x00000000, LENGTH = 0x00010000 */
DATA (rw)  : ORIGIN = 0x00200000, LENGTH = 0x00004000
}

PROVIDE ( __int_ram_top__ = ORIGIN(DATA) + LENGTH(DATA) );


/* Section Definitions */

SECTIONS
{
/* first section is .text which is used for code */
. = ORIGIN(CODE);
.text :
{
*(.reset .reset.*)
*(.text  .text.*)
*(.gnu.linkonce.t.*)
*(.glue_7t) *(.glue_7)
*(.rodata .rodata.*)
*(.gnu.linkonce.r.*)
} >CODE = 0xFF

. = ALIGN(4);
___text_end__ = . ;
PROVIDE (__text_end__ = .);

/* .data section which is used for initialized data */

.data : AT (___text_end__)
{
__data_start__ = . ;
*(.data .data.*)
*(.gnu.linkonce.d*)
SORT(CONSTRUCTORS)
. = ALIGN(4);
__end__ = . ;
PROVIDE (__data_end__ = .);
} >DATA

/* .bss section which is used for uninitialized data */

.bss :
{
__bss_start = . ;
__bss_start__ = . ;
*(.bss .bss.*)
*(.gnu.linkonce.b*)
*(COMMON)
. = ALIGN(4);
__bss_end__ = . ;
}

_end = .;
PROVIDE (end = .);

/* Stabs debugging sections.  */
.stab          0 : { *(.stab) }
.stabstr       0 : { *(.stabstr) }
.stab.excl     0 : { *(.stab.excl) }
.stab.exclstr  0 : { *(.stab.exclstr) }
.stab.index    0 : { *(.stab.index) }
.stab.indexstr 0 : { *(.stab.indexstr) }
.comment       0 : { *(.comment) }
/* DWARF debug sections.
Symbols in the DWARF debugging sections are relative to the beginning
of the section so we begin them at 0.  */
/* DWARF 1 */
.debug          0 : { *(.debug) }
.line           0 : { *(.line) }
/* GNU DWARF 1 extensions */
.debug_srcinfo  0 : { *(.debug_srcinfo) }
.debug_sfnames  0 : { *(.debug_sfnames) }
/* DWARF 1.1 and DWARF 2 */
.debug_aranges  0 : { *(.debug_aranges) }
.debug_pubnames 0 : { *(.debug_pubnames) }
/* DWARF 2 */
.debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
.debug_abbrev   0 : { *(.debug_abbrev) }
.debug_line     0 : { *(.debug_line) }
.debug_frame    0 : { *(.debug_frame) }
.debug_str      0 : { *(.debug_str) }
.debug_loc      0 : { *(.debug_loc) }
.debug_macinfo  0 : { *(.debug_macinfo) }
/* SGI/MIPS DWARF 2 extensions */
.debug_weaknames 0 : { *(.debug_weaknames) }
.debug_funcnames 0 : { *(.debug_funcnames) }
.debug_typenames 0 : { *(.debug_typenames) }
.debug_varnames  0 : { *(.debug_varnames) }

}