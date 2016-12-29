#define _function r4
#define argc r5
#define argt r6
#define _argv r7
#define argi r8
#define temp r9

/* uint32_t fmr_call(const void *function, uint8_t argc, uint16_t argt, void *argv); */

.syntax unified
.global fmr_call
.func fmr_call
.thumb_func
fmr_call:
	/* Save registers. */
	push { _function-temp, lr }
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
	mov temp, argt
	and temp, 3
	/* Jump to the appropraite code for the argument's type. */
	cmp temp, #0
	beq _load_8
	cmp temp, #1
	beq _load_16
	cmp temp, #2
	beq _load_32
	/* If the type was encoded improperly, return with error. */
	b _error
_load_8:
	/* Load a byte from the argument list. */
	ldrb temp, [_argv, #0]
	/* Increment the argv pointer by one. */
	add _argv, #1
	b _load_done
_load_16:
	/* Load a halfword from the argument list. */
	ldrh temp, [_argv, #0]
	/* Increment the argv pointer by two. */
	add _argv, #2
	b _load_done
_load_32:
	/* Load a word from the argument list. */
	ldr temp, [_argv, #0]
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
	mov r0, temp
	b _write_done
_write_r1:
	mov r1, temp
	b _write_done
_write_r2:
	mov r2, temp
	b _write_done
_write_r3:
	mov r3, temp
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
	b _done
_error:
	/* Return -1 if an error has occured. */
	mov r0, #-1
_done:
	/* Restore registers and return to the top-level caller. */
	pop { _function-temp, pc }

.endfunc
.end
