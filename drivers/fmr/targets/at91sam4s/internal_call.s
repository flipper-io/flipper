# uint32_t internal_call(void *function, uint8_t argc, void *argv)

.thumb

.global internal_call

internal_call:	push { r4-r5, lr }

				mov r5, r0

				mov r4, r2

				ldmia r4!, { r0 - r3 }

				ldr r4, =check_done
				add r4, r4, #1
				mov lr, r4

				bx r5

check_done:

				pop { r4-r5, pc }
