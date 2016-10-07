.section .vectors

.align 2

.word _stack // Stack Address
.word _reset // 1 Reset
.word _exit  // 2 NMI
.word _exit  // 3 HardFault
.word _exit  // 4 MemManage
.word _exit  // 5 BusFault
.word _exit  // 6 UsageFault
.word _exit  // 7 RESERVED
.word _exit  // 8 RESERVED
.word _exit  // 9 RESERVED
.word _exit  // 10 RESERVED
.word _exit  // 11 SVCall
.word _exit  // 12 Debug Monitor
.word _exit  // 13 RESERVED
.word _exit  // 14 PendSV
.word _exit  // 15 SysTick
.word _exit  // 16 External Interrupt(0)
.word _exit  // 17 External Interrupt(1)
.word _exit  // 18 External Interrupt(2)
.word _exit  // 19 ...

.thumb
.text
.align

.global _reset
.thumb_func
.global system_init
.global system_task
_reset:
	bl system_init
	bl system_task
@_copy_data:
@	# Copy the data section from Flash to SRAM.
@	ldr r1, =__text_end__
@	ldr r2, =__data_start__
@	ldr r3, =__data_end__
@_copy_data_loop:
@	cmp r2, r3
@	bge _zero_bss
@	ldr r0, [r1]
@	add r1, r1, #4
@	str r0, [r2]
@	add r2, r2, #4
@	b _copy_data_loop
@_zero_bss:
@	# Zero the BSS section.
@	movs r0, #0
@	ldr r1, =__bss_start__
@	ldr r2, =__bss_end__
@_zero_bss_loop:
@	cmp r1, r2
@	bge _call_main
@	str r0, [r1]
@	add r1, r1, #4
@	b _zero_bss_loop
@	bl system_init
@	bl system_task
@	b _exit

# Provide an exit loop for the firmware.
.global _exit
_exit:
	b .
