%include "constants.asm"
%include "syscalls.asm"
%include "integer_utils.asm"
%include "string_utils.asm"


N_ROWS equ 1734
N_ITEMS_PER_ROW equ 3

section .bss
  input_buffer resb 32768
  input_bufsize equ $ - input_buffer
  output_buffer resb 1024
  output_bufsize equ $ - output_buffer
  numbers resd N_ROWS * N_ITEMS_PER_ROW

section .data
  input_file db 'data/day3.txt',0x0
  unknown_input_error db 'Unknown input',0xa,0x0
  unknown_input_error_len equ $ - unknown_input_error

section .text
  global _start


;; args:
;; - zero terminated string
;; - destination byte array
;; returns:
;; - eax number of integers
read_integers:
  cld

  push ebp
  mov ebp, esp

  push esi
  push edi

  mov esi, [FIRST_OF_TWO_ARGS]
  mov edi, [SECOND_OF_TWO_ARGS]

  call read_whitespace

.loop:
  cmp DWORD [esi], 0x0
  je .return

  call read_integer
  stosd

  call read_whitespace
  jmp .loop
.return:
  mov eax, edi
  sub eax, [SECOND_OF_TWO_ARGS]
  shr eax, 2                    ; n longs read = n bytes read / 4

  pop edi
  pop esi

  mov esp, ebp
  pop ebp
  ret


;; args: 3 numbers
;; returns:
;; - eax: 0x1 if triangle, 0x0 otherwise
is_triangle:
  push ebp
  mov ebp, esp

  mov eax, [FIRST_OF_THREE_ARGS]
  add eax, [SECOND_OF_THREE_ARGS]
  cmp eax, [THIRD_OF_THREE_ARGS]
  jle .not_a_triangle

  mov eax, [FIRST_OF_THREE_ARGS]
  add eax, [THIRD_OF_THREE_ARGS]
  cmp eax, [SECOND_OF_THREE_ARGS]
  jle .not_a_triangle

  mov eax, [SECOND_OF_THREE_ARGS]
  add eax, [THIRD_OF_THREE_ARGS]
  cmp eax, [FIRST_OF_THREE_ARGS]
  jle .not_a_triangle

  mov eax, 0x1
  jmp .return
.not_a_triangle:
  mov eax, 0x0
.return:
  mov esp, ebp
  pop ebp
  ret


;; args:
;; - number array
;; - number of numbers
;; vars:
;; - triangle count
;; returns:
;; - count of triangles
day3_part1:
  push ebp
  mov ebp, esp

  push ebx
  push edx
  push esi

  mov DWORD [FIRST_VAR], 0

  mov esi, [FIRST_OF_TWO_ARGS]
  mov eax, [SECOND_OF_TWO_ARGS]
  mov edx, 0
  mov ebx, 3
  div ebx

  mov ecx, eax
.loop:
  push DWORD [esi+0*4]
  push DWORD [esi+1*4]
  push DWORD [esi+2*4]
  call is_triangle
  add esp, 3*4
  add DWORD [FIRST_VAR], eax
.next:
  add esi, 3*4
  loop .loop

  mov eax, [FIRST_VAR]

  pop esi
  pop edx
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - number array
;; - number of numbers
;; vars:
;; - triangle count
;; returns:
;; - count of triangles
day3_part2:
  push ebp
  mov ebp, esp

  push ebx
  push edx
  push esi

  mov DWORD [FIRST_VAR], 0

  mov esi, [FIRST_OF_TWO_ARGS]
  mov eax, [SECOND_OF_TWO_ARGS]
  mov edx, 0
  mov ebx, 9
  div ebx

  mov ecx, eax
.loop:
  push DWORD [esi+0*4]
  push DWORD [esi+3*4]
  push DWORD [esi+6*4]
  call is_triangle
  add esp, 3*4
  add DWORD [FIRST_VAR], eax

  push DWORD [esi+1*4]
  push DWORD [esi+4*4]
  push DWORD [esi+7*4]
  call is_triangle
  add esp, 3*4
  add DWORD [FIRST_VAR], eax

  push DWORD [esi+2*4]
  push DWORD [esi+5*4]
  push DWORD [esi+8*4]
  call is_triangle
  add esp, 3*4
  add DWORD [FIRST_VAR], eax
.next:
  add esi, 9*4
  loop .loop

  mov eax, [FIRST_VAR]

  pop esi
  pop edx
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args: none
;; vars:
;; - n integers read
;; returns: nothing
_start:
  push ebp
  mov ebp, esp

  sub esp, 1*4

  push input_file               ; read input file
  push input_buffer
  push input_bufsize
  call read_file
  add esp, 3*4

  push input_buffer
  push numbers
  call read_integers
  add esp, 2*4
  mov [FIRST_VAR], eax

  mov ecx, [FIRST_VAR]
  mov esi, numbers

  push numbers
  push DWORD [FIRST_VAR]
  call day3_part1
  add esp, 2*4

  push eax
  call print_integer
  add esp, 1*4

  push numbers
  push DWORD [FIRST_VAR]
  call day3_part2
  add esp, 2*4

  push eax
  call print_integer
  add esp, 1*4

  push DWORD 0
  call sys_exit
