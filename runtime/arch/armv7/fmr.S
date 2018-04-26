#define _function r5
#define retv r6
#define argc r7
#define argt r8
#define _argv r9
#define argi r10
#define temp r11

/* lf_return_t fmr_call(lf_return_t (* function)(void), lf_type ret, uint8_t argc, uint16_t argt, void *argv); */

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
	cmp argc, #0
	beq _call

	mov temp, argt
	and temp, 0xF

	# lf_int8_t
	cmp temp, #0
		beq _load_8
	# lf_int16_t
	cmp temp, #1
		beq _load_16
	# lf_int32_t
	cmp temp, #3
		beq _load_32
	# lf_int_t
	cmp temp, #4
		beq _load_ptr
	# lf_ptr_t
	cmp temp, #6
		beq _load_int
	# lf_int64_t
	cmp temp, #7
		beq _load_64

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
_load_64:
	b _failure

_load_int:
_load_ptr:
	ldr temp, [_argv, #0]
	add _argv, #8

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
	b _failure

_write_done:
	lsrs argt, #4
	sub argc, #1
	add argi, #1
	b _load

_call:
	/* Jump to the function. */
	blx _function

_ret:
	# lf_int8_t
	cmp retv, #0
		beq _ret_8
	# lf_int16_t
	cmp retv, #1
		beq _ret_16
	# lf_void_t
	cmp retv, #2
		beq _ret_void
	# lf_int32_t
	cmp retv, #3
		beq _ret_32
	# lf_int_t
	cmp retv, #4
		beq _ret_int
	# lf_int_t
	cmp retv, #6
		beq _ret_ptr
	# fmr_int
	cmp retv, #7
		beq _ret_64

	b _failure

_ret_void:
	mov r0, #0
	mov r1, #0
	b _call_done
_ret_8:
	b _call_done
_ret_16:
	b _call_done
_ret_int:
_ret_ptr:
_ret_32:
	b _call_done
_ret_64:
	b _call_done

_failure:
	mov r0, #-1
	mov r1, #-1
_call_done:
	pop { _function-temp, pc }

.endfunc
.end
