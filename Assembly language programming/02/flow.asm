default rel

global start
global step

section .bss
w       resd    1
size    resq    1
arr     resq    1
weight  resd    1        


section .text

; Initialise the simulation: save the array's width and total size.
; Arguments:
; edi - array's width
; esi - array's height
; rdx - pointer to the array
; xmm0 -  weight
start:
    mov             [w], edi
    mov             [arr], rdx
    imul            rdi, rsi
    mov             [size], rdi
    movss           [weight], xmm0
    ret

; Perform one step of the simulation
; Arguments:
; rdi - pointer to the array containing the new elements of the leftmost column
step:
	push            rbp
    sub             rsp, 32
    vmovdqu         [rsp], ymm6             ; push ymm6
	mov		        rbp, rsp 		        ; save stack pointer
    mov             rsi, [arr]              ; rsi := pointer to array
    movss           xmm0, [rsi + 4]         ; duplicate first value
    movss           [rsi], xmm0             ; save it in offset as first cell's left neighbour, which doesn't affect the calculated delta
    add             rsi, 4                  ; skip offset
    mov             rdx, [size]             ; rdx := size
    mov             r8d, [w]                ; r8 := width
    mov             r9d, r8d
    imul            r9, -1                  ; r9 := -width
    lea             r10, [rdx + r9]         ; r10 := size - width 
    lea             rax, [rdx * 4]
    sub             rsp, rax                ; move top of stack by 4 * size for delta array

    xor             ecx, ecx
replace_loop:
    movss           xmm0, [rdi]             
    movss           [rsi + 4 * rcx], xmm0   ; update the leftmost column with new values

    add             rdi, 4
    add             rcx, r8
    cmp             rcx, rdx
    jb              replace_loop 

    vbroadcastss    ymm6, [weight]
    xor             ecx, ecx
loop:                                       ; NOTE: The calculations are incorrect for the leftmost column, but it's overwritten later on anyway
    lea             rax, [rsi + 4 * rcx] 
    vmovups         ymm0, [rax]             ; cells
    vmovups         ymm1, [rax - 4]         ; left neighbours
    vsubps          ymm1, ymm0

    cmp             rcx, r8
    jb              first_row_skip
    
    vmovups         ymm2, [rax + 4 * r9]    ; upper neighbours
    vmovups         ymm3, [rax + 4 * r9 - 4]; left upper neighbours
    vsubps          ymm2, ymm0
    vsubps          ymm3, ymm0
    vaddps          ymm1, ymm2 
    vaddps          ymm1, ymm3 

first_row_skip:
    cmp             rcx, r10
    jae             last_row_skip

    vmovups         ymm4, [rax + 4 * r8 - 4]; left lower neighbours
    vmovups         ymm5, [rax + 4 * r8]    ; lower neighbours
    vsubps          ymm4, ymm0
    vsubps          ymm5, ymm0
    vaddps          ymm1, ymm4 
    vaddps          ymm1, ymm5

last_row_skip:
    vmulps          ymm1, ymm6
    vmovups         [rsp + 4 * rcx], ymm1   ; save calculated deltas in delta array

    add             rcx, 8
    cmp             rcx, rdx
    jb              loop

    xor             ecx, ecx
add_loop:
    vmovups         ymm0, [rsp + 4 * rcx]
    vaddps          ymm0, [rsi + 4 * rcx]
    vmovups         [rsi + 4 * rcx], ymm0   ; update the array

    add             rcx, 8
    cmp             rcx, rdx
    jb              add_loop    

    mov             rsp, rbp                ; revert top of the stack
    vmovdqu         ymm6, [rsp]             ; pop ymm6
    add             rsp, 32
    pop             rbp
    ret