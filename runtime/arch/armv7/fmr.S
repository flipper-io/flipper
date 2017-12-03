#define _function r5
#define retv r6
#define argc r7
#define argt r8
#define _argv r9
#define argi r10
#define temp r11

/* lf_return_t fmr_call(lf_return_t (* function)(void), fmr_type ret, uint8_t argc, uint16_t argt, void *argv); */

.syntax unified
.global fmr_call
.func fmr_call
.thumb_func
fmr_call:
	/* Save registers. */
	push { r5-r11, lr }
	/* Save the function address into r4. */
	mov _function, r0
	/* Load the return type. */
	mov retv, r1
	/* Load the argument count into r5. */
	mov argc, r2
	/* Load the argument types into r6. */
	mov argt, r3
	/* Load the address of the argument list into r7. */
	ldr _argv, [sp, #32]
	/* Clear the argument index register. */
	mov argi, #0
_load:
	/* Check if the argument count is zero. */
	cmp argc, #0
	/* If there are no arguments left to load, call the function. */
	beq _call
	/* Obtain the argument type from the lowest two bits by masking 0b1111. */
	mov temp, argt
	and temp, 0xF
	/* Jump to the appropraite code for the argument's type. */
	cmp temp, #0
		beq _load_8
	cmp temp, #1
		beq _load_16
	cmp temp, #2
		beq _load_32
	cmp temp, #3
		beq _load_ptr
	cmp temp, #4
		beq _load_int
	/* If the type was encoded improperly, return with error. */
	b _failure
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
	b _load_done
_load_ptr:
	b _load_32
_load_int:
	b _load_32
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
	b _write_done
_write_stack:
	/* Todo. */
	b _failure
_write_done:
	/* Shift the encoded types two bits to the right. */
	lsrs argt, #4
	/* Decrease the argument count. */
	sub argc, #1
	/* Increase the argument index. */
	add argi, #1
	/* Contiunue loading arguments. */
	b _load
_call:
	/* Jump to the function. */
	blx _function
_ret:
	/* Obtain the argument type from the lowest two bits by masking 0b11. */
	mov temp, retv
	and temp, 0xF
	/* Jump to the appropraite code for the argument's type. */
	cmp temp, #0
		beq _ret_8
	cmp temp, #1
		beq _ret_16
	cmp temp, #2
		beq _ret_32
	cmp temp, #3
		beq _ret_ptr
	cmp temp, #4
		beq _ret_int
	cmp temp, #5
		beq _ret_void
	b _failure
_ret_8:
	b _call_done
_ret_16:
	b _call_done
_ret_32:
	b _call_done
_ret_ptr:
	b _call_done
_ret_int:
	b _call_done
_ret_void:
	mov r0, #0
	b _call_done
_failure:
	/* Return -1 if an error has occured. */
	mov r0, #-1
_call_done:
	/* Restore registers and return to the top-level caller. */
	pop { _function-temp, pc }

.endfunc
.end
