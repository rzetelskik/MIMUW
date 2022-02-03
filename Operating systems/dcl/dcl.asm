SYS_READ  equ 0
SYS_WRITE equ 1 
SYS_EXIT  equ 60
STDIN     equ 0
STDOUT    equ 1
NULL      equ 0
ARGC      equ 5                     ; Required number of arguments.
DC        equ 42                    ; Length of a drum.
DC2       equ 84                    ; Doubled length of a drum.
SPAN      equ 41                    ; Span of all accepted symbols.
MINC      equ 49                    ; Minimal ascii code of an acceptable character. 
BC        equ 16384                 ; Length of buffer used for reading the input.
ROT1      equ 27                    ; First rotational position.
ROT2      equ 33                    ; Second rotational position.
ROT3      equ 35                    ; Third rotational position.

%define LKEY r12d
%define RKEY r13d

; Normalises a char stored at rax registry.
; %1 - char to be normalised.
; Modifies rax registry. All the other registers are left untouched.
%macro norm 1
  movzx   eax, %1
  sub     eax, MINC  
  cmp     al, SPAN
  ja      exit_1
%endmacro

; Applies the permutation at the second argument with positive offset
; at the first argument to the character stored in rax register.
; %1 - positive offset.
; %2 - permutation array.
; Modifies rax registry. All the other registers are left untouched.
%macro prm 2
  movzx   eax, byte [%2 + eax + %1] 
%endmacro

; Applies the permutation at the second argument with negative offset
; at the first argument to the character stored in rax register.
; %1 - negative offset.
; %2 - permutation array.
; Modifies rax registry. All the other registers are left untouched.
%macro prminv 2
  sub     eax, %1
  movzx   eax, byte [%2 + eax + DC]
%endmacro

global _start

section .bss

; All permutation arrays are doubled in size to avoid modulo operation.

L resb DC2                          ; Normalised left drum array.
R resb DC2                          ; Normalised right drum array.
T resb DC2                          ; Normalised reverse drum array.

LINV resb DC2                       ; Normalised inverted left drum array.
RINV resb DC2                       ; Normalised inverted right drum array.
TINV resb DC2                       ; Normalised inverted reverse drum array.

; Doubled identity array to avoid modulo operation.
M resb DC2

section .text

; Call sys_exit with status code 1. 
exit_1:
  mov     eax, SYS_EXIT
  mov     edi, 1                    ; Set exit code to 1.
  syscall                           ; Call sysexit.

; Validate a drum and save it and its inverse simultanously.
; Check whether the drum's length is DC and if all the characters are within the accepted range.
; Check whether it's a valid permutation by inverting it. If it is not valid
; or of unaccepted range, call exit_1.
; Save the normalised permutation and its inverse at addresses provided.
; rdi - an address to save the normalised drum at.
; rsi - source, the address of the argument containing the permutation.
; rdx - an address to save the INVERSE of the normalised drum at.
; The procedure modifies the registers rax and rcx. All the other registers 
; are left untouched.
parse_drum:
  xor     ecx, ecx                  ; Reset register for loop counter.
.loop:
  norm    byte [rsi + rcx]          ; Normalise char and save it at eax.
  mov     byte [rdi + rcx], al      ; Append normalised char to array.
  mov     byte [rdi + rcx + DC], al ; Append copy to the doubled part of the array.
  cmp     byte [rdx + rax], 0       ; Check if the value hasn't been set yet.
  jnz     exit_1                    ; If it has, permutation is invalid.
  mov     byte [rdx + rax], cl      ; Append it to inverted array.
  mov     byte [rdx + rax + DC], cl ; Append copy to the doubled part of the inverted array.
  add     ecx, 1                    ; Increment loop counter.
  cmp     ecx, DC                 
  jne     .loop                     ; Loop over characters until counter reaches drum length.

  cmp     byte [rsi + rcx], NULL    ; Check if null comes right after DC bytes.
  jne     exit_1                    ; If the argument string is longer than DC, call exit_1.
  ret

; Check whether T drum is valid, e.i. for any i in range [0, 42) : i != T[i] = TINV[i].
; The procedure modifies the registers rax and rcx. All the other registers 
; are left untouched.
; Procedure takes no arguments.
; Procedure modifies registers rcx and rax. All the other registers 
; are left untouched.
check_tdrum:
  xor     ecx, ecx                  ; Set counter to zero.
.loop:
  cmp     byte [T + ecx], cl        ; Check if T[i] = i.
  je      exit_1                    ; If true, call exit(1).
  movzx   eax, byte [TINV + ecx]    ; Get T[T[i]].
  cmp     byte [T + ecx], al        ; Check if T[T[i]] = T[i].
  jne     exit_1                    ; If false, call exit(1).
  add     ecx, 1                    ; Increment counter.
  cmp     ecx, DC                   ; Check if the end of drum was reached.
  jne     .loop                     ; Otherwise loop.
  ret

; Normalise the l and r keys, check if they are in valid character range
; and save them in the registers for further use.
; rsi - source, the address of the argument containing the keys.
; Procedure modifies the rax register. All the other registers 
; are left untouched.
parse_key:
  norm    byte [rdi]                ; Normalise l key and save it at rax.
  mov     LKEY, eax                 ; Save l key at r12d.
  norm    byte [rdi + 1]            ; Normalise r key and save it at rax.
  mov     RKEY, eax                 ; Save r key at r13d.
  cmp     byte [rdi + 2], NULL      ; Check if there is no further characters.
  jne     exit_1                    ; Otherwise call sys_exit with code 1.
  ret

; Populate the doubled identity array used to avoid modulo operations.
; Procedure modifies the rcx register. All the other registers 
; are left untouched.
fill_mod:
  xor     ecx, ecx
.loop:
  mov     byte [M + ecx], cl        ; M[i] = i % DC;
  mov     byte [M + DC + ecx], cl   ; M[i + DC] = i % DC;
  add     ecx, 1                    ; Increment counter.
  cmp     ecx, DC                   
  jnz     .loop                     ; Loop over characters until counter reaches drum length.
  ret

_start:
  cmp     qword [rsp], ARGC         ; Compare the number of arguments to ARGC.
  jne     exit_1                    ; exit(1) if it differs.

  mov     rsi, qword [rsp + 16]     ; Get the first argument.
  mov     rdi, L                    
  mov     rdx, LINV
  call    parse_drum                ; Validate L drum, normalise it, save it and its inverse.

  mov     rsi, qword [rsp + 24]     ; Get the second argument.
  mov     rdi, R                  
  mov     rdx, RINV
  call    parse_drum                ; Validate R drum, normalise it, save it and its inverse.

  mov     rsi, qword [rsp + 32]     ; Get the third argument.
  mov     rdi, T
  mov     rdx, TINV
  call    parse_drum                ; Validate T drum, normalise it, save it and its inverse.
  call    check_tdrum               ; Check whether T drum meets the additional criteria.

  mov     rdi, qword [rsp + 40]     ; Get the fourth argument.
  call    parse_key                 ; Check wheter it contains a valid key and save it in registers.
  call    fill_mod                  ; Populate the identinty array used to avoid modulo operation.

  sub     rsp, BC                   ; Allocate BC bytes for the i/o buffer on stack.

loop_read:
  mov     eax, SYS_READ
  mov     edi, STDIN
  mov     rsi, rsp                  ; Provide the address of the buffer on stack.
  mov     edx, BC                   ; Provide the length of the buffer.
  syscall                           ; Read BC bytes from stdin to buffer.
  
  cmp     eax, 0                    ; Check for end of input.
  je      exit_0                    ; If no input was read, call sysexit with code 0.
  mov     r8d, eax                  ; Save read size at r8.
  xor     ecx, ecx                  ; Reset counter to 0

loop_char:
  add     RKEY, 1
  movzx   RKEY, byte [M + RKEY]     ; Rotate RKEY by one position.
  cmp     RKEY, ROT1                ; Test whether RKEY reached the first rotational position.
  je      rotate_l
  cmp     RKEY, ROT2                ; Test whether RKEY reached the second rotational position.
  je      rotate_l
  cmp     RKEY, ROT3                ; Test whether RKEY reached the third rotational position.
  jne     skip                     
rotate_l:                           ; If RKEY reached a rotational position, rotate LKEY.
  add     LKEY, 1
  movzx   LKEY, byte [M + LKEY]     ; Rotate LKEY by one position.
skip:
  norm    byte [rsp + rcx]          ; Take the next character from buffer on stack and save it at rax.
  prm     RKEY, R                   ; Apply Q(r)R permutations.
  prminv  RKEY, M                   ; Apply Q^-1(r) permutation
  prm     LKEY, L                   ; Apply Q(l)R permutations.
  prminv  LKEY, T                   ; Apply Q^-1(l)T permutations.
  prm     LKEY, LINV                ; Apply Q(l)L^-1 permutations.
  prminv  LKEY, M                   ; Apply Q^-1(l) permutation.
  prm     RKEY, RINV                ; Apply Q(r)R^-1 permutations.
  prminv  RKEY, M                   ; Apply Q^-1(r) permutation.

  add     al, MINC                  ; Denormalise char.
  mov     byte [rsp + rcx], al      ; Save denormalised char in buffer on stack.
  add     ecx, 1                    ; Increment counter.
  cmp     ecx, r8d                  ; Compare counter with read size.
  jne     loop_char                 ; Read next char if counter < read size.

  mov     eax, SYS_WRITE
  mov     edi, STDOUT
  mov     rsi, rsp                  ; Provide the address of the Buffer on stack.
  mov     edx, r8d                  ; Provide the length of the data read.
  syscall                           ; Write encoded string to stdout.

  jmp     loop_read                 ; Read next part of input.
  
exit_0:
  mov     eax, SYS_EXIT
  xor     edi, edi                  ; Set exit code 0.
  syscall                           ; Call sysexit.