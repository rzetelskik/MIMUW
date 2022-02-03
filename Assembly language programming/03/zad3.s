.data

.balign 4
minval:
    .byte 0
.balign 4
maxval:
    .byte 255

.text
.global modify_component

@ Modify component by a given value in [-127, 127].
@ r0 - pointer to matrix
@ r1 - width
@ r2 - height
@ r3 - component to be modified
@ sp - additive value
modify_component:
    push    {r4, r5}
    ldrsb   r4, [sp, #8]    @ move value to r4
    muls    r1, r2          @ r1 = width * height, i.e. size
    beq     end             @ terminate if size == 0

    sub     r3, #1          
    mla     r0, r1, r3, r0  @ r0 - pointer to subarray to be modified

    ldr     r3, addr_maxval
    ldr     r3, [r3]

    ldr     r5, addr_minval
    ldr     r5, [r5]

    @ r1 = idx
loop:
    sub     r1, #1          @ idx--
    ldrb    r2, [r0, r1]    @ r2 = subarray[idx]
    adds    r2, r4          @ r2 += value
    cmp     r2, r3
    movgt   r2, r3          @ r2 = min(r2, maxval)
    cmp     r2, r5
    movlt   r2, r5          @ r2 = max(r2, minval)
    strb    r2, [r0, r1]    @ subarray[idx] = r2

    teq r1, #0
    bne loop                @ while (idx)

end:
    pop {r4, r5}
    bx lr

addr_minval: .word minval
addr_maxval: .word maxval
