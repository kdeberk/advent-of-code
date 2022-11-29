%ifndef __INTEGER_UTILS_ASM__
%define __INTEGER_UTILS_ASM__

; Division (by non-powers-of-2) is expensive in CPU land, so we prefer to use
; multiplication and right-shifting (division by powers-of-2) instead. To divide
; by 5, we can multiply by 4/5 and then divide by 4.
;   x/5 = x*1/5 = (x*4/5)/4
; The following constant is 2^64 multiplied by 4/5. Not sure why 2^64 and not
; sure why we're only interested in the upper 64 bits of the 128-bit result.
DIV_BY_5_MAGIC equ -3689348814741910323 ; 2^64 * 4/5. Multiplication by 2^64

section .bss
    intBuffer resb 20
    intBufferEnd resb 1

section .text

;; uintToString(uint, bytes) uint
;; rdi: n, rsi: dst, rax: n bytes
;; Copies the string representation of the integer to the given buffer, returns
;; the number of bytes copied.
uintToString:
    push rbp
    mov rbp, rsp
    sub rsp, 8                  ; room for current location buffer

    mov rcx, rdi
    mov [rsp], rsi

    std                         ; set direction flag, stosb will now decrement
.loop:
    mov rdi, rcx
    call mod10

    add rax, '0'
    mov rdi, [rsp]
    stosb                       ; write al to [rdi] and decrement rdi
    mov [rsp], rdi
                                ;
    mov rdi, rcx
    call div10
    mov rcx, rax

    test rcx, rcx               ; check if n is 0
    je uintToString_end         ; we're done

    jmp .loop                   ; continue loop
uintToString_end:
    cld                         ; reset direction flag.

    mov rsp, rbp
    pop rbp
    ret

;; mod10(uint) uint
;; rdi: n, rax: n%10
mod10:
    push rbp
    mov rbp, rsp

    mov rdx, DIV_BY_5_MAGIC  ; rdx = 4/5*2^64
    mov rax, rdi
    mul rdx                     ; rdx = n*4/5
    shr rdx, 3                  ; rdx = (n*4/5)/8 = n*(1/10)
    mov rax, rdx                ; rax = n*(1/10)
    sal rax, 2                  ; rax = n*(4/10)
    add rax, rdx                ; rax = n*(5/10)
    add rax, rax                ; rax = n*(10/10) = n, after integer truncation
    mov rdx, rax
    mov rax, rdi
    sub rax, rdx                ; n%10 = n-(n/10)

    mov rsp, rbp
    pop rbp
    ret

;; mod10(uint) uint
;; rdi: n, rax: n/10
div10:
    push rbp
    mov rbp, rsp

    mov rdx, DIV_BY_5_MAGIC  ; rdx = 4/5*2^64
    mov rax, rdi
    mul rdx                     ; rdx = n*4/5
    shr rdx, 3                  ; rdx = (n*4/5)/8 = n*(1/10)
    mov rax, rdx                ; rax = n*(1/10)

    mov rsp, rbp
    pop rbp
    ret

%endif
