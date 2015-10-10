.equ AIC_IVR, (256)
.equ AIC_FVR, (260)
.equ AIC_EOICR, (304)
.equ AT91C_BASE_AIC, (0xFFFFF000)
.equ	IRQ_STACK_SIZE, (3*8*4)
.equ	ARM_MODE_FIQ, 0x11
.equ	ARM_MODE_IRQ, 0x12
.equ	ARM_MODE_SVC, 0x13
.equ	I_BIT, 0x80
.equ	F_BIT, 0x40
.equ   IRQ_Stack_Size,     0x00000060

.code 32

.align 0

.section .reset, "ax"

reset:			b begin

undefined:		b undefined

swi:			b swi

pabt:			b pabt

dabt:			b dabt

reserved:		b reserved

irq:			b irq_handler

fiq:			b fiq

begin:			ldr r13, =__int_ram_top__

				ldr r0, =boot

				mov lr, pc

				bx r0

				mov r0,r13

                msr CPSR_c, #ARM_MODE_FIQ | I_BIT | F_BIT

            	ldr r8, =AT91C_BASE_AIC

                msr CPSR_c, #ARM_MODE_IRQ | I_BIT | F_BIT

                mov r13, r0

                sub r0, r0, #IRQ_Stack_Size

                msr CPSR_c, #ARM_MODE_SVC

                mov r13, r0

/* relocate .data section (copy from rom to ram) */

                ldr r1, =__text_end__

                ldr r2, =__data_start__

                ldr r3, =__data_end__

looprel:        cmp r2, r3

                ldrlo r0, [r1], #4

                strlo r0, [r2], #4

                blo looprel

/* clear .bss section (zero init) */

                mov r0, #0

                ldr r1, =__bss_start__

                ldr r2, =__bss_end__

loopzi:         cmp r1, r2

                strlo r0, [r1], #4

                blo loopzi

.extern main

				ldr	lr, =stop

				ldr	r0, =main

				bx	r0

stop:			b stop

irq_handler:	sub		lr, lr, #4

				stmfd	sp!, { lr }

				mrs		r14, SPSR

				stmfd	sp!, { r14 }

				stmfd	sp!, { r0 }

				ldr		r14, =AT91C_BASE_AIC

				ldr		r0 , [r14, #AIC_IVR]

				str		r14, [r14, #AIC_IVR]

				msr		CPSR_c, #ARM_MODE_SVC

				stmfd	sp!, { r1-r3, r12, r14 }

				mov		r14, pc

				bx		r0

				ldmia	sp!, { r1-r3, r12, r14}

				msr		CPSR_c, #I_BIT | ARM_MODE_IRQ

				ldr		r14, =AT91C_BASE_AIC

				str		r14, [r14, #AIC_EOICR]

				ldmia	sp!, { r0 }

				ldmia	sp!, { r14 }

				msr		SPSR_cxsf, r14

				ldmia	sp!, { pc }^
