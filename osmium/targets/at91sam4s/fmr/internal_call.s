# uint32_t internal_call(void *function, uint8_t argc, void *argv)

.thumb

.global internal_call

internal_call:	mov r12, r0

				mov r4, r2

				ldmia r4!, { r0 - r3 }

				bx r12
