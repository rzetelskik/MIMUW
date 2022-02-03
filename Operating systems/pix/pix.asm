extern pixtime
global pix

BASE      equ 16                    ; Base used in the calculations.

; Use iterative exponentiation by squaring to calculate 16^(n-k) mod 8k + j.
; rsi - mod value (8k + j)
; r15 - (n - k)
; Modifies rax, rdx, r8, r9 and r10 registers. Saves the result in rdx register.
%macro ebs 0
  xor     r9d, r9d
  cmp     rsi, 1
  setne   r9b                       ; result = ((8k + j) != 1)
  test    r15, r15                  
  je      %%fin                     ; Finish if exponent is equal to 0.
  mov     r10d, BASE                ; base = 16
  mov     r8d, 1                    ; counter = 1
%%loop:
  mov     rax, r10
  xor     edx, edx
  div     rsi                       
  mov     r10, rdx                  ; base = base mod (8k + j)
  test    r15, r8                   
  je      %%skip                    ; Skip multiplication if counter & exponent.
  mov     rax, r9                   
  imul    rax, rdx
  xor     edx, edx
  div     rsi
  mov     r9, rdx                   ; result = (result * base) mod (8k + j)
%%skip:
  add     r8, r8                    ; Double the counter.
  imul    r10, r10                  ; base *= base
  cmp     r15, r8                   
  jnb     %%loop                    ; Loop if i <= n - k.
%%fin:
  mov     rdx, r9                   ; Save result at rdx.
%endmacro


section .text

; Get the time stamp counter (TSC) and call the external procedure pixtime.
; Procedure takes no parameters. It modfies rax, rdi and rdx registers.
pixrd:
  rdtsc                             ; Get current TSC and save it at edx:eax.
  mov   edi, edx
  shl   rax, 32
  shld  rdi, rax, 32                ; Save edx:eax at rdi.
  call  pixtime
  ret


; Calculate Sj. See below for explanation:
; (https://math.stackexchange.com/questions/880904/how-do-you-use-the-bbp-formula-to-calculate-the-nth-digit-of-%CF%80)
; rdi - n parameter
; rsi - j parameter
; Procedure modifies rax, rdx, r8, r9, r10, r11 and r15 registers. The result is returned in rax register.
calc_sj:
  xor     r11d, r11d                ; Zero the accumulator.
  mov     r15, rdi                  ; Set counter to n.
; Calculate first partial sum from n to 0, step 1.
sum1:                               
  ebs                               ; Calculate the numerator and save it at rdx.
  xor     eax, eax                  
  div     rsi                       ; Divide the numerator by 8k + j.
  add     r11, rax                  ; Add the partial result.
  add     rsi, 8                    
  sub     r15, 1                    ; Decrease counter by 1.
  jns     sum1                      ; Finish if counter < 0.

  mov     r8d, 1                    
  shl     r8, 60                    ; Save 1/16 at r8.
sum2:
  xor     edx, edx
  mov     rax, r8                   
  div     rsi                       ; Divide the numerator by (8k + j).
  add     r11, rax                  ; Add the partial result.  
  
  add     rsi, 8                      
  shr     r8, 4                     ; Divide the numerator by 16.
  test    r8, r8                    
  jne     sum2                      ; Finish if there are no more set bits in the numerator.
  mov     rax, r11
  ret


; Calculate [32m + 1, 32m + 31] bits of the fractional part of pi binary expansion
; using the Bailey-Borweim-Plouffe Formula.
; rdi - address of the destination array
; rsi - address of the current index
; rdx - max index
; Procedure is thread-safe and compliant with ABI.
pix: 
  push    r12
  push    r13
  push    r14
  push    r15
  mov     r12, rdi                  ; Save the address of the destination array.
  mov     r13, rsi                  ; Save the address of the index.
  mov     r14, rdx                  ; Save the max index.
  call    pixrd                     

loop:
  mov     edi, 1
  lock \
  xadd    qword [r13], rdi          ; Get the current index, save the copy at rdi and increment the original by one.
  cmp     rdi, r14                  
  jae     quit                      ; Quit if the index (before incrementation) is greater or equal to max value. 

  shl     rdi, 3                    ; m = 8 * index
  xor     ecx, ecx                  ; Zero the result.
  mov     esi, 1                    
  call    calc_sj                   ; Calculate S1.
  lea     rcx, [rcx + 2*rax]        ; result = 2S1.
  mov     esi, 4                    
  call    calc_sj                   ; Calculate S4.
  sub     rcx, rax                  ; result = 2S1 - S4 
  add     rcx, rcx                  ; result = 4S1 - 2S4
  mov     esi, 5                    
  call    calc_sj                   ; Calculate S5.
  sub     rcx, rax                  ; result = 4S1 - 2S4 - S5
  mov     esi, 6
  call    calc_sj                   ; Calculate S6.
  sub     rcx, rax                  ; result = 4S1 - 2S4 - S5 - S6
  shr     rcx, 32                   

  shr     rdi, 1                    ; m = 4 * index. Correct memory address.
  mov     dword [r12 + rdi], ecx    ; Save the result in the destination array.
  jmp     loop

quit:
  call    pixrd
  pop     r15
  pop     r14
  pop     r13
  pop     r12
  ret




