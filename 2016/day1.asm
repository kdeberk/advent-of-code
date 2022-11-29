%include 'integer_utils.asm'
%include 'string_utils.asm'
%include 'syscalls.asm'

section .data
    greeting db 'Hello, World!',0x0

section .bss
    outputBuffer resb 100
    outputBufferEnd resb 0

section .text
    global _main

_main:
    ;; mov rdi, greeting
    ;; call strlen

    ;; mov rdi, greeting
    ;; mov rsi, rax
    ;; call writeStderr

    mov rdi, 23
    mov rsi, outputBufferEnd
    sub rsi, 1
    call uintToString

    mov rdi, outputBuffer
    add rdi, 98
    mov rsi, 2
    call writeStdout

    mov rdi, 0
    call exit
