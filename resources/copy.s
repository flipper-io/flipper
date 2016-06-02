.global start
.global stack
.global reset
.global destination
.global source
.global words
.text
.thumb
.align 0

start:
    # Load the destination address into r0.
    ldr     r0, destination
    # Load the source address into r1.
    ldr     r1, source
    # Load the number of words into r2.
    ldr     r2, words
    # Branch to the check function.
    b       check
    
copy:
    # Load the value at the source address (r1) into r3.
    ldmia   r1!, {r3}
    # Store the value in r3 at the destination address (r0).
    stmia   r0!, {r3}
    # Subtract one from the word count.
    sub     r2, #1
    
check:
    # If all the words haven't been copied, go back to the copy function.
    cmp     r2, #0
    bne     copy
    # Check to see if the reset vector is zero.
    ldr     r0, reset
    cmp     r0, #0
    bne     return
    # Set the stack pointer.
    ldr     r0, stack
    mov     sp, r0

# Go back to the SAM-BA.
return:
    bx      lr

# Allocate space to store the supporting data.

.align  0
# Locate the stack
stack:
    .word   0x00000000
reset:
    .word   0x0000000
destination:
    .word   0x000000
source:
    .word   0x000000
# This is set to 0x20 because 32 words is equal to 128 bytes. (1 page write.)
words:
    .word   0x000000

