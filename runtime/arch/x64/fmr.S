# lf_return_t fmr_call(lf_return_t (* function)(void), uint8_t ret, uint8_t argc, uint16_t argt, void *argv);

#define argc %r9
#define argt %r10
#define argi %r11

#define ret %r12
#define argv %r13
#define temp %r14

.global _fmr_call

.text

_fmr_call:
	push %rbp
	mov %rsp, %rbp
	push %r14
	push %r13
	push %r12

	/* Put the function pointer on the stack. */
	subq $8, %rsp
	movq %rdi, -8(%rbp)

	mov %rsi, ret
	mov %rdx, argc
	mov %rcx, argt
	mov %r8, argv
	mov $0, argi

_load:
	cmp	$0, argc
	je _do_call

	movq argt, temp
	and $0xF, temp
	cmp $0, temp
		je _load_8
	cmp $1, temp
		je _load_16
	cmp $2, temp
		je _load_32
	cmp $3, temp
		je _load_ptr
	cmp $4, temp
		je _load_int
	jmp _failure

_load_8:
	movb (argv), %al
	add $1, argv
	jmp _load_done
_load_16:
	movw (argv), %ax
	add $2, argv
	jmp _load_done
_load_32:
	movl (argv), %eax
	add $4, argv
	jmp _load_done
_load_64:
	movq (argv), %rax
	add $8, argv
	jmp _load_done
_load_ptr:
	jmp _load_64
_load_int:
	jmp _load_32

_load_done:
	mov argi, temp
	and $0xF, temp
	cmp $0, temp
		je _write_rdi
	cmp $1, temp
		je _write_rsi
	cmp $2, temp
		je _write_rdx
	cmp $3, temp
		je _write_rcx
	cmp $4, temp
		je _write_r8
	cmp $4, temp
		je _write_r9
	jmp _write_stack

_write_rdi:
	movq %rax, %rdi
	jmp _write_done
_write_rsi:
	movq %rax, %rsi
	jmp _write_done
_write_rdx:
	movq %rax, %rdx
	jmp _write_done
_write_rcx:
	movq %rax, %rcx
	jmp _write_done
_write_r8:
	movq %rax, %r8
	jmp _write_done
_write_r9:
	movq %rax, %r9
	jmp _write_done
_write_stack:
	jmp _failure

_write_done:
	shr $4, argt
	sub $1, argc
	add $1, argi
	jmp _load

_do_call:
	callq *-8(%rbp)
	addq $8, %rsp
	jmp _done

_failure:
	mov $-1, %rax
_done:
	pop %r12
	pop %r13
	pop %r14
	pop %rbp
	retq
