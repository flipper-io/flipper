.thumb

.text

stack:
.word 0x00000000

entry:
.word 0x00000000

start:
    # Load the destination address (in flash) into r0.
    ldr r0, destination
    # Load the source address (in RAM) into r1.
    ldr r1, source
    # Load the number of words to copy into r2.
    ldr r2, words
    # Check if there still remain words left to copy.
    b check
copy:
    # Load the value at the address stored in r1 into r3 and increment r1 by 4.
    ldmia r1!, { r3 }
    # Store the value in r3 at the address stored in r0 and increment r0 by 4.
    stmia r0!, { r3 }
    # Subtract one from the number of words remaining.
    sub r2, #1
check:
    # Check if r2 is zero.
    cmp r2, #0
    # If r2 is greater than zero, continue to copy words.
    bne copy
write:
    # Load the address of the EEFC -> FCR.
    ldr r0, fcr0
    # Write the key (0x5a) and EWP command (0x03) into the EEFC -> FCR.
    ldr r1, ewp
    # Write the page into flash memory.
    str r1, [r0, #0]
wait:
    # Load the value of the EEFC -> FSR.
    ldr r0, fsr0
    ldr r0, [r0, #0]
    # Wait until the EEFC has finished writing the page.
    mov r1, #1
    tst r0, r1
    bne wait
done:
    # Return to the caller.
    bx lr

.align 4

# Holds the address within flash memory to which words will be copied to.
destination:
    .word 0x00000000
# Holds the address within RAM from which words will be copied from.
source:
    .word 0x00000000
# Stores the number of words copied by the applet each time it is run.
words:
    .word 128

# Stores the addresses of the important flash registers.
fcr0:
    .word 0x400E0A04
fsr0:
    .word 0x400E0A08
ewp:
    .word 0x5a000003
