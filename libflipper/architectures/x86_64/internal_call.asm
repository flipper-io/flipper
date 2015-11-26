section .text
	global _internal_call

; void internal_call(void *addr, uint8_t argc, uint16_t argv[argc])
; argv is assumed to point to a buffer with a capacity of at least
; four bytes.
; At call:
; addr -> rdi
; argc -> rsi
; argv -> rdx
_internal_call:	push rbp         ; Save caller base pointer.
		mov rbp, rsp     ; Caller's stack top is our base.
		push rbx         ; Save caller's rbx;
		push r12         ; Save caller's r12.
		push r13         ; Save caller's r13.
		push r14         ; Save caller's r14.
		mov r10, rdi     ; Save addr
		mov r11, rsi     ; Save argc
		mov r12, rdx     ; Save argv
		mov rdx, rsp
		and rdx, 0xf     ; rsp % 16
		neg rdx
		add rdx, 16
		mov r13, rdx     ; Save number of bytes added to stack.
		sub rsp, rdx     ; Align the stack.
		mov rax, 0       ; rax will buffer the arguments.
		cmp r11, 6       ; argc >= 6?
		jae arg6         ; If so, we're moving all 6 args.
		mov r14, 0       ; r14 is the jump table offset.
		sub r14, r11     ; Subtract the number of arguments.
		shl r14, 2       ; r14 * 12 = (r14 << 2) * 3
		lea r14, [r14*3]
		lea rbx, [rel arg0]
		lea rbx, [rbx + r14]
		jmp rbx          ; Jump into the table.
arg6:		mov ax, [r12 + 5*2]
		mov r9, rax
		dec r11
arg5:		mov ax, [r12 + 4*2]
		mov r8, rax
		dec r11
arg4:		mov ax, [r12 + 3*2]
		mov rcx, rax
		dec r11
arg3:		mov ax, [r12 + 2*2]
		mov rdx, rax
		dec r11
arg2:		mov ax, [r12 + 1*2]
		mov rsi, rax
		dec r11
arg1:		mov ax, [r12]
		mov rdi, rax
		dec r11
		nop
arg0:		cmp r11, 0   ; Are we done handling arguments?
		je subcall   ; If so, do the call.
		test r11, 1  ; Check if number of args left is odd.
		jnz argstack ; If it's odd, we don't need to align rsp.
		sub rsp, 8   ; If it's even, keep the stack 16 byte aligned.
		add r13, 8   ; Increment stack block flag.
argstack:	mov ax, [r12 + 10 + 2*r11]
		push rax
		add r13, 8
		dec r11
		cmp r11, 0
		jne argstack
subcall:	call r10       ; Perform the call.
		mov [r12], eax ; Save the 4 byte return value at argv.
		add rsp, r13 ; Clean up stack frame.
		pop r14
		pop r13 ; Restore caller's r13.
		pop r12 ; Restore caller's r12.
		pop rbx ; Restore caller's rbx.
		pop rbp ; Restore caller's rbp.
		ret     ; Return.
