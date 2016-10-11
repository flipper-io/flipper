#define _function r4
#define argc r5
#define argt r6
#define _argv r7
#define argi r9

.thumb
.syntax unified

/* uint32_t fmr_call(const void *function, uint8_t argc, uint16_t argt, void *argv); */

.global fmr_call

fmr_call:
	/* Save registers. */
	push { r4-r9, lr }
	/* Save the function address into r4. */
	mov _function, r0
	/* Load the argument count into r5. */
	mov argc, r1
	/* Load the argument types into r6. */
	mov argt, r2
	/* Load the address of the argument list into r7. */
	mov _argv, r3
	/* Clear the argument index register. */
	mov argi, #0
_load:
	/* Check if the argument count is zero. */
	cmp argc, #0
	/* If there are no arguments left to load, call the function. */
	beq _call
	/* Obtain the argument type from the lowest two bits by masking 0b11. */
	mov r8, argt
	and r8, 3
	/* Jump to the appropraite code for the argument's type. */
	cmp r8, #0
	beq _load_8
	cmp r8, #1
	beq _load_16
	cmp r8, #2
	beq _load_32
_load_8:
	/* Load a byte from the argument list. */
	ldrb r8, [_argv, #0]
	/* Increment the argv pointer by one. */
	add _argv, #1
	b _load_done
_load_16:
	/* Load a halfword from the argument list. */
	ldrh r8, [_argv, #0]
	/* Increment the argv pointer by two. */
	add _argv, #2
	b _load_done
_load_32:
	/* Load a word from the argument list. */
	ldr r8, [_argv, #0]
	/* Increment the argv pointer by four. */
	add _argv, #4
_load_done:
	/* If the argument index is less than 4, load the argument into the appropriate register. */
	cmp argi, #0
	beq _write_r0
	cmp argi, #1
	beq _write_r1
	cmp argi, #2
	beq _write_r2
	cmp argi, #3
	beq _write_r3
	b _write_stack
_write_r0:
	mov r0, r8
	b _write_done
_write_r1:
	mov r1, r8
	b _write_done
_write_r2:
	mov r2, r8
	b _write_done
_write_r3:
	mov r3, r8
_write_stack:
	/* Todo. */

_write_done:
	/* Shift the encoded types two bits to the right. */
	lsrs argt, #2
	/* Decrease the argument count. */
	sub argc, #1
	/* Increase the argument index. */
	add argi, #1
	/* Contiunue loading arguments. */
	b _load
_call:
	/* Jump to the function. */
	blx _function
	/* Restore registers and return to the top-level caller. */
	pop { r4-r9, pc }
