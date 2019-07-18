%include "constants.asm"
%include "syscalls.asm"

section .bss
  input_buffer resb 1024
  input_bufsize equ $ - input_buffer
  output_buffer resb 1024
  output_bufsize equ $ - output_buffer

section .data
  input_file db 'data/day2.txt',0x0

section .text
  global _start


_start:
  push ebp
  mov ebp, esp


  mov eax, 0
  push eax
  call sys_exit
  ; no ret
