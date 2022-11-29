%ifndef __SYSCALLS__
%define __SYSCALLS__

;; Syscalls in Mac OS X works as follows:
;; - Syscall values start at 2000000
;; - Parameters are in registers
;;   rax, rdi, rsi, rdx, r8, r9

section .text

STDOUT equ 1
STDERR equ 2
SYSCALL_EXIT  equ 0x2000001
SYSCALL_WRITE equ 0x2000004

;; writeStderr(string, n)
;; rdi: string; rsi: n
;; Writes n bytes of the given string to stderr.
writeStderr:
    push rbp
    mov rbp, rsp

    mov rdx, rsi                ; set length
    mov rsi, rdi                ; set string
    mov rdi, STDERR             ; set fd
    mov rax, SYSCALL_WRITE      ; set syscall
    syscall

    mov rsp, rbp
    pop rbp
    ret

;; writeStdout(string, n)
;; rdi: string; rsi: n
;; Writes n bytes of the given string to stdout.
writeStdout:
    push rbp
    mov rbp, rsp

    mov rdx, rsi                ; set length
    mov rsi, rdi                ; set string
    mov rdi, STDOUT             ; set fd
    mov rax, SYSCALL_WRITE      ; set syscall
    syscall

    mov rsp, rbp
    pop rbp
    ret


exit:
    mov rax, SYSCALL_EXIT
    syscall

%endif
