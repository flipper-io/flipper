.intel_syntax noprefix

.text

.globl internal_call

internal_call:	push ebp

				mov ebp, esp

				push edi

				mov edi, [ebp + 8]

				mov eax, [ebp + 12]

				mov ecx, [ebp + 16]

				push esi

				push ebx

				push ecx

				dec eax

				mov ebx, eax

				shl ebx, 2

				add ecx, ebx

				inc eax

				mov esi, eax

callL:			cmp eax, 0

				je callR

				mov ebx, [ecx]

				push ebx

				sub ecx, 4

				dec eax

				jmp callL

callR:			call edi

callC:			cmp esi, 0

				je callD

				add esp, 4

				dec esi

				jmp callC

callD:			pop ecx

				mov [ecx], eax

				pop ebx

				pop esi

				pop edi

				mov esp, ebp

				pop ebp

				ret
