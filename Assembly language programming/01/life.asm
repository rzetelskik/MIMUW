default rel

global start
global run

; Check whether the neighbour at index in rsi is alive (value is an odd number)
; and add it in to the neighbour counter at al.
; Modifies rax and r9 registers. All other registers are left intact.
%macro count_neighbour 0
	mov		r9b, byte [r8 + rsi]
	and		r9b, 0x1
	add		al, r9b
%endmacro

ALIVE equ 35
DEAD  equ 32
NULL  equ 0

section .bss

w	 resd 1					; Width of the array, excluding the terminating zero.
size resq 1					; Size of the array including the terminating zeros.
arr  resq 1					; Pointer to the provided array.

section .text

; Initialise the simulation: save the array's width and total size.
; Arguments:
; edi - array's width
; esi - array's height
; rdx - pointer to the array
start:
	mov		[w], edi		; Save arrays
	mov		[arr], rdx		; Save pointer to the array
	add		edi, 1			; Account space for zero terminating each row
	imul	rdi, rsi		
	mov		[size], rdi		; size := (width + 1) * height
    ret

; Run the simulation the number of times provided in arguments.
; Arguments:
; edi - number of generations to simulate
run: 
	test	edi, edi			
	jle		end				; jump to end of simulation unless gen > 0

	push	r12				
	push	rbp
	mov		rbp, rsp 		; save stack pointer
	mov		r10d, [w]		; r10d := width of the array
	mov		r11, [size]		; r11 := size of the array
	lea		r12, [r10 - 1]	; r12 := width of the array - 1, used in simulations
	sub		rsp, r11 		; move top of stack by size bytes for second array
	mov		r8, [arr]		; r8 := pointer to original array
	
	; r8 - current generation, rsp - generation being computed
	mov		rcx, r11		; rcx := size
	sub		rcx, 1
	add		r10d, 1			; r10d = width + 1 (add guard)
	; fill last byte of each row in second array with \0, which will serve as a guard
loop_zeros:
	mov		byte [rsp + rcx], 0
	sub		rcx, r10		
	jns		loop_zeros
	sub		r10d, 1

	xchg	r8, rsp
gen_loop:
	xchg	r8, rsp			; swap pointers to arrays
	xor		ecx, ecx 		; rcx - array loop iterator
loop:
	xor		eax, eax		; rax - neighbours counter
	mov		r9b, byte [r8 + rcx]
	test	r9b, r9b
	jz		move			; \0 guard, skip simulation for this cell	

	mov		rsi, rcx		; rsi - index of the neighbour being visited
	sub		rsi, 1			; left neighbour
	js		successors		; skip all neighbours with lower indexes if out of bounds
	count_neighbour

	sub		rsi, r12		; top right neighbour
	js		successors		; skip all neighbours with lower indexes if out of bounds
	count_neighbour

	sub		rsi, 1			; top neighbour
	js		successors		; skip all neighbours with lower indexes if out of bounds
	count_neighbour

	sub		rsi, 1			; top left neighbour
	js		successors		; skip all neighbours with lower indexes if out of bounds
	count_neighbour

successors:
	mov		rsi, rcx		; rsi - index of the neighbour being visited
	add		rsi, 1			; right neighbour
	cmp		rsi, r11		
	jge		check			; skip all neighbours with higher indexes if out of bounds
	count_neighbour

	add		rsi, r12		; bottom left neighbour
	cmp		rsi, r11
	jge		check			; skip all neighbours with higher indexes if out of bounds
	count_neighbour

	add		rsi, 1			; bottom neighbour
	cmp		rsi, r11
	jge		check			; skip all neighbours with higher indexes if out of bounds
	count_neighbour

	add		rsi, 1			; bottom right neighbour
	cmp		rsi, r11
	jge		check			; skip all neighbours with higher indexes if out of bounds
	count_neighbour

	; determine cell's state in the next generation
check:
	mov		esi, DEAD
	cmp		eax, 3
	jg		set
	mov		esi, ALIVE
	je		set
	cmp		eax, 2
	mov		esi, DEAD
	cmove	esi, [r8 + rcx]

	; set cell's determined state in the next generation
set:
	mov		byte [rsp + rcx], sil

	; move to the next cell
move:
	add		rcx, 1
	cmp		rcx, r11
	jl		loop			; continue array loop unless out of bounds

	sub		rdi, 1  		
	jnz		gen_loop		; continue simulation unless finished

	cmp		r8, [arr]		; check whether current array is the original array
	jne		skip_copy

	xor ecx, ecx
	xor eax, eax
copy_loop:
	mov		al, byte [rsp + rcx]
	mov		[r8 + rcx], al
	add		rcx, 1
	cmp		rcx, r11
	jl		copy_loop
skip_copy:
	mov		rsp, rbp		; reset stack's top
	pop		rbp				
	pop		r12
end:
    ret


