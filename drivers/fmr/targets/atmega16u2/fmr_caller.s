#include <avr/common.h>

#define argv_lo							r20

#define argv_hi							r21

#define argc_lo							r22

#define argc_hi							r23

#define address_lo						r24

#define address_hi						r25

.func									internal_call

.global									internal_call

internal_call:							; Push all of the registers we will be using onto the stack.

										push ZH

										push ZL

										push XH

										push XL

										push r6

										push r5

										push r4

										push r3

										push r2

										; XL has to be 26, because it will be pre-decremented to 25 before the first loop.

										ldi XL, 26

										clr XH

										; r5 will keep track of how many arguments still need to be loaded.

										mov r5, argc_lo

										; r6 will save argc_lo.

										mov r6, argc_lo

										; Subtract one from r6 to ensure the uppermost byte of the 32-bit return value isn't popped.

										; dec r6

										; The Z register will store a pointer to a buffer in main memory which will contain the arugments.

										mov ZL, argv_lo

										mov ZH, argv_hi

										; R1 and R2 will store the address of the function which we wish to call after the arguments have been loaded.

										mov r2, address_lo

										mov r3, address_hi

_load_argument_list_loop:				; We need to perform the compare before we decrement the argument counter in the event that argc is 0.

										tst r5

										; If we've reached zero, we're done.

										breq _load_argument_list_complete

										; Load -X into r4.

										ld r4, -X

										; Save the register to the stack.

										push r4

										; r3 = *Z. Increment Z.

										ld r4, Z+

										; Decrement X. *X = r3.

										st X, r4

										; If not, subtract one from r5. (argc).

										dec r5

										; Jump back to the loop and repeat.

										rjmp _load_argument_list_loop

_load_argument_list_complete:			; Save the X register.

										push XH

										push XL

										; Save the X register again.

										push XH

										push XL

										; Save the Z register.

										push ZH

										push ZL

										; Load the function's address into the Z register.

										mov ZL, r2

										mov ZH, r3

										; Call the function.

										icall

										; Restore the Z register.

										pop ZL

										pop ZH

										; Restore the X register.

										pop XL

										pop XH

										; Get back to the original address;

										sub ZL, r6

										; XL has to be 26, because it will be pre-decremented to 25 before the first loop.

										ldi XL, 22

_return_loop:							; See if we're done.

										cpi XL, 26

										; If we are, be done.

										breq _return_complete

										; Save the byte into the array.

										ld r4, X+

										st Z+, r4

										; Loop.

										rjmp _return_loop

_return_complete:						; Restore the X register.

										pop XL

										pop XH

_restore_registers_loop:				; We need to perform the compare before we decrement the argument counter in the event that argc is 0.

										tst r6

										; If we've reached zero, we're done.

										breq _restore_registers_complete

										; Pop the register.

										pop r4

										; *X = r3. Increment x;

										st X+, r4

										; If not, subtract one from r6. (argc).

										dec r6

										; Jump back to the loop and repeat.

										rjmp _restore_registers_loop

_restore_registers_complete:			; Unload the remainder of the used registers from the stack.

										pop r2

										pop r3

										pop r4

										pop r5

										pop r6

										pop XL

										pop XH

										pop ZL

										pop ZH

										; Return to the parent function.

										ret