.syntax unified
.thumb

/*
	Exception frame saved by the NVIC hardware onto stack:
	+------+
	|      | <- SP before interrupt (orig. SP)
	| xPSR |
	|  PC  |
	|  LR  |
	|  R12 |
	|  R3  |
	|  R2  |
	|  R1  |
	|  R0  | <- SP after entering interrupt (orig. SP + 32 bytes)
	+------+

	Registers saved by the software (pendsv_exception):
	+------+
	|  R7  |
	|  R6  |
	|  R5  |
	|  R4  |
	|  R11 |
	|  R10 |
	|  R9  |
	|  R8  | <- Saved SP (orig. SP + 64 bytes)
	+------+
*/

.global pendsv_exception
.type pendsv_exception, %function
pendsv_exception:
	/* Disable interrupts. */
	cpsid i

	/*
	   Save registers R4-R11 (32 bytes) onto current PSP (process stack
	   pointer) and make the PSP point to the last stacked register (R8):
	   - The MRS/MSR instruction is for loading/saving a special registers.
	   - The STMIA inscruction can only save low registers (R0-R7), it is
	     therefore necesary to copy registers R8-R11 into R4-R7 and call
	     STMIA twice.
	*/
	mrs r0, psp
	subs r0, #16
	stmia r0!, {r4-r7}
	mov r4, r8
	mov r5, r9
	mov r6, r10
	mov r7, r11
	subs r0, #32
	stmia r0!, {r4-r7}
	subs r0, #16

	/* Save current task's SP. */
	ldr r2, =os_current_task
	ldr r1, [r2]
	str r0, [r1]

	/* Load next task's SP. */
	ldr r2, =os_next_task
	ldr r1, [r2]
	ldr r0, [r1]

	/*
	   Load registers R4-R11 (32 bytes) from the new PSP and make the PSP
	   point to the end of the exception stack frame. The NVIC hardware
	   will restore remaining registers after returning from exception).
	*/
	ldmia r0!, {r4-r7}
	mov r8, r4
	mov r9, r5
	mov r10, r6
	mov r11, r7
	ldmia r0!,{r4-r7}
	msr psp, r0

	/* EXC_RETURN (thread mode with PSP) */
	ldr r0, =0xFFFFFFFD

	/* Enable interrupts. */
	cpsie i

	/* Branch to EXC_RETURN. */
	bx r0
