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

  push input_file               ; read input file
  push input_buffer
  push input_bufsize
  call read_file
  add esp, 3*4
  mov [FIRST_VAR], eax

  mov eax, 0
  push eax
  call sys_exit
  ; no ret
