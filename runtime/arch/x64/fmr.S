# lf_return_t fmr_call(lf_return_t (* function)(void), uint8_t ret, uint8_t argc, uint16_t argt, void *argv);

#define argc %r9
#define argt %r10
#define argi %r11

#define retv %r12
#define argv %r13
#define temp %r14

#ifdef __APPLE__
#define FMR_CALL _fmr_call
#else
#define FMR_CALL fmr_call
#endif

.global FMR_CALL

.text

FMR_CALL:
	push %rbp
	mov %rsp, %rbp
	push %r14
	push %r13
	push %r12
	subq $8, %rsp

	/* Put the function pointer on the stack. */
	movq %rdi, -8(%rbp)

	mov %rsi, retv
	mov %rdx, argc
	mov %rcx, argt
	mov %r8, argv
	mov $0, argi

_load:
	cmp	$0, argc
	je _do_call

	movq argt, temp
	and $0x7, temp

	# lf_int8_t
	cmp $0, temp
		je _load_8
	# lf_int16_t
	cmp $1, temp
		je _load_16
	# lf_int32_t
	cmp $3, temp
		je _load_32
	# lf_int_t
	cmp $4, temp
		je _load_int
	# lf_ptr_t
	cmp $6, temp
		je _load_ptr
	# lf_int64_t
	cmp $7, temp
		je _load_64

	jmp _fail

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
_load_int:
_load_ptr:
_load_64:
	movq (argv), %rax
	add $8, argv
	jmp _load_done

_load_done:
	cmp $0, argi
		je _write_rdi
	cmp $1, argi
		je _write_rsi
	cmp $2, argi
		je _write_rdx
	cmp $3, argi
		je _write_rcx
	cmp $4, argi
		je _write_r8
	cmp $5, argi
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
	jmp _fail

_write_done:
	shr $4, argt
	sub $1, argc
	add $1, argi
	jmp _load

_do_call:
	callq *-8(%rbp)

_ret:

	movq retv, temp
	and $0x7, temp

	# lf_int8_t
	cmp $0, temp
		je _ret_8
	# lf_int16_t
	cmp $1, temp
		je _ret_16
	# lf_void_t
	cmp $2, temp
		je _ret_void
	# lf_int32_t
	cmp $3, temp
		je _ret_32
	# lf_int_t
	cmp $4, temp
		je _ret_int
	# lf_ptr_t
	cmp $6, temp
		je _ret_ptr
	# lf_int64_t
	cmp $7, temp
		je _ret_64

	jmp _fail

_ret_8:
	jmp _done
_ret_16:
	jmp _done
_ret_void:
	jmp _done
_ret_32:
	jmp _done
_ret_int:
	jmp _done
_ret_ptr:
	jmp _done
_ret_64:
	jmp _done

_fail:
	mov $-1, %rax
_done:
	addq $8, %rsp
	pop %r12
	pop %r13
	pop %r14
	pop %rbp
	retq
