#define param_function_h	r25
#define param_function_l	r24
#define param_argc_h		r23
#define param_argc_l		r22
#define argc				r2
#define param_argt_h		r21
#define param_argt_l		r20
#define	argt_h				r5
#define argt_l				r4
#define param_argv_h		r19
#define param_argv_l		r18

; uint32_t fmr_call(void *function, uint8_t argc, uint16_t argt, void *argv);

.func fmr_call
.global fmr_call

fmr_call:
	; Preserve the callee saved registers.
	push argc
	push argt_l
	push argt_h
	; Preserve the function address on the stack.
	push param_function_h
	push param_function_l
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
	; Check if the argument count is zero.
	tst argc
	; If there are no arguments left to load, call the function.
	breq _call
	; Obtain the argument type from the lowest two bits by masking 0b11.
	mov r16, argt_l
	andi r16, 3
	; Jump to the appropraite code for the arguments type.
	cpi r16, 0
	breq _load_8
	cpi r16, 1
	breq _load_16
	cpi r16, 2
	breq _load_32
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
_load_done:
	; Shift the encoded types two bits to the right.
	asr argt_h
	ror argt_l
	asr argt_h
	ror argt_l
	; Decrease the argument count.
	dec argc
	; Contiunue loading arguments.
	rjmp _load
_call:
	; Retrieve the function address from the stack.
	pop ZL
	pop ZH
	; Call the target function.
	icall
_call_done:
	; Restore the callee saved registers.
	pop argt_h
	pop argt_l
	pop argc
	; Return to the caller.
	ret
