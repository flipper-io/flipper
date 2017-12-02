# lf_return_t fmr_call(lf_return_t (* function)(void), uint8_t argc, uint16_t argt, void *argv);

.global _fmr_call

.data

format: .ascii "fmr_call: f:%p c:0x%02x t:0x%04x v:%p\n"

.text



_fmr_call:
	push %rbp
	mov %rsp, %rbp

	mov %rsi, -8(%rbp)
	mov %rdx, -12(%rbp)
	mov %rcx, -16(%rbp)
	mov %r8, -20(%rbp)
	mov $0x0, %al

	lea format(%rip), %rdi
	callq _printf

	jmp _call_done

_load_8:
	jmp _load_done
_load_16:
	jmp _load_done
_load_32:
	jmp _load_done
_load_ptr:
	jmp _load_32
_load_done:

_call:

_call_done:

	pop %rbp
	retq
