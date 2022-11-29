%ifndef __STRING_UTILS_ASM__
%define __STRING_UTILS_ASM__

section .text

;; strlen(string) int
;; Returns the length of string, including the final 0x00 byte.
strlen:
    push rbp
    mov rbp, rsp

    mov rsi, rdi

.loop:
    lodsb                       ; load next byte into al
    cmp al, 0x00
    je .end                     ; rsi is at pos of 0x00
    jmp .loop

.end:
    mov rax, rsi                ; subtract pos of 0x00 from start.
    sub rax, rdi

    mov rsp, rbp
    pop rbp
    ret

%endif
