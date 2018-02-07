#include <avr/io.h>

#define param_function_h	r25
#define param_function_l	r24
#define param_retv_h			r23
#define param_retv_l			r22
#define retv				r2
#define param_argc_h		r21
#define param_argc_l		r20
#define argc				r3
#define param_argt_h		r19
#define param_argt_l		r18
#define	argt_h				r5
#define argt_l				r4
#define param_argv_h		r17
#define param_argv_l		r16

; lf_return_t fmr_call(lf_return_t (* function)(void), lf_type ret, uint8_t argc, uint16_t argt, void *argv);

.global fmr_call

fmr_call:
	; Preserve the callee saved registers.
	push argc
	push argt_l
	push argt_h
	; Preserve the function address on the stack.
	push param_function_h
	push param_function_l
	; Save the retv parameter into a lower register.
	mov retv, param_retv_l
	; Save the argc parameter into a lower register.
	mov argc, param_argc_l
	; Save the encoded argument types into a lower register duet.
	movw argt_l, param_argt_l
	; Save the argv pointer into the Z register.
	movw ZL, param_argv_l
	; Load the address of register 25 into the X register.
	clr XH
	ldi XL, 25

_load:
	tst argc
	breq _call

	mov r16, argt_l
	andi r16, 0x7

	; lf_int8_t
	cpi r16, 0
		breq _load_8
	; lf_int16_t
	cpi r16, 1
		breq _load_16
	; lf_int32_t
	cpi r16, 3
		breq _load_32
	; lf_int_t
	cpi r16, 4
		breq _load_int
	; lf_ptr_t
	cpi r16, 6
		breq _load_ptr
	; lf_int64_t
	cpi r16, 7
		breq _load_64

	rjmp _failure

_load_8:
	ld r0, Z+
	st -X, r0
	dec XL
	rjmp _load_done
_load_16:
	dec XL
	ld r0, Z+
	st X+, r0
	ld r0, Z+
	st X, r0
	subi XL, 0x02
	rjmp _load_done
_load_32:
	subi XL, 0x03
	ld r0, Z+
	st X+, r0
	ld r0, Z+
	st X+, r0
	ld r0, Z+
	st X+, r0
	ld r0, Z+
	st X, r0
	subi XL, 0x04
	rjmp _load_done
_load_64:
	rjmp _failure

_load_int:
_load_ptr:
	dec XL
	ld r0, Z+
	st X+, r0
	ld r0, Z+
	st X, r0
	subi XL, 0x02
	adiw ZL, 0x06
	rjmp _load_done

_load_done:
	asr argt_h
	ror argt_l
	asr argt_h
	ror argt_l
	asr argt_h
	ror argt_l
	asr argt_h
	ror argt_l
	dec argc
	rjmp _load

_call:
	; Retrieve the function address from the stack.
	pop ZL
	pop ZH
	; Call the target function.
	icall

_ret:

	mov r16, retv
	andi r16, 0x7

	; lf_int8_t
	cpi r16, 0
		breq _ret_8
	; lf_int16_t
	cpi r16, 1
		breq _ret_16
	; lf_void_t
	cpi r16, 2
		breq _ret_void
	; lf_int32_t
	cpi r16, 3
		breq _ret_32
	; lf_int_t
	cpi r16, 4
		breq _ret_int
	; lf_ptr_t
	cpi r16, 6
		breq _ret_16
	; lf_int64_t
	cpi r16, 7
		breq _ret_64

	rjmp _failure

_ret_8:
	clr r25
	mov r22, r24
	clr r24
	clr r23
	sbrs r16, 4
	rjmp _done
	sbrs r22, 7
	rjmp _done
	com r25
	com r24
	com r23
	rjmp _done
_ret_int:
	sbr r16, 4
_ret_16:
	mov r23, r25
	clr r25
	mov r22, r24
	clr r24
	sbrs r16, 4
	rjmp _done
	sbrs r23, 7
	rjmp _done
	com r25
	com r24
	rjmp _done
_ret_32:
	rjmp _done
_ret_64:
	rjmp _failure
_ret_void:
	clr r25
	clr r24
	clr r23
	clr r22
	rjmp _done

_failure:
	ser r25
	ser r24
	ser r23
	ser r22
_done:
	; Restore the callee saved registers.
	pop argt_h
	pop argt_l
	pop argc
	; Return to the caller.
	ret
