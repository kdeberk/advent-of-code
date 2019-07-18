%include "constants.asm"
%include "syscalls.asm"

section .bss
  input_buffer resb 4096
  input_bufsize equ $ - input_buffer
  output_buffer resb 1024
  output_bufsize equ $ - output_buffer

section .data
  input_file db 'data/day2.txt',0x0
  unknown_direction_error db 'Unknown direction error.',0xa,0x0
  unknown_direction_error_len equ $ - unknown_direction_error
  part1_keypad db 0x0,0x0,0x0,0x0,0x0,\
                  0x0,"123",0x0,\
                  0x0,"456",0x0,\
                  0x0,"789",0x0,\
                  0x0,0x0,0x0,0x0,0x0
  part1_keypad_start_x equ 2
  part1_keypad_start_y equ 2
  part1_keypad_width equ 5
  part2_keypad db 0x0,0x0,0x0,0x0,0x0,0x0,0x0,\
                  0x0,0x0,0x0,"1",0x0,0x0,0x0,\
                  0x0,0x0,"234",0x0,0x0,\
                  0x0,"56789",0x0,\
                  0x0,0x0,"ABC",0x0,0x0,\
                  0x0,0x0,0x0,"D",0x0,0x0,0x0,\
                  0x0,0x0,0x0,0x0,0x0,0x0,0x0,
  part2_keypad_start_x equ 1
  part2_keypad_start_y equ 3
  part2_keypad_width equ 7

section .text
  global _start


;; args:
;; - x position
;; - y position
;; - keypad
;; - keypad width
;; returns:
;; - eax: button value
current_button:
  push ebp
  mov ebp, esp

  push ebx
  push ebx  

  mov edx, 0

  mov eax, [SECOND_OF_FOUR_ARGS] ; calculate keypad[y*width+x]
  mov ebx, [FOURTH_OF_FOUR_ARGS]
  imul eax, ebx
  add eax, [FIRST_OF_FOUR_ARGS]
  add eax, [THIRD_OF_FOUR_ARGS]

  mov eax, [eax]

  pop edx
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - instructions, zero-terminated string
;; - start x position
;; - start y position
;; - keypad
;; - width of keypad
;; vars:
;; - current x position
;; - current y position
;; - next x position
;; - next y position
;; returns:
;; - eax: number of buttons pressed.
day2_part1:
  cld

  push ebp,
  mov ebp, esp

  sub esp, 4*4

  mov esi, [FIRST_OF_FIVE_ARGS]
  mov edi, output_buffer

  mov eax, [SECOND_OF_FIVE_ARGS]
  mov [FIRST_VAR], eax
  mov eax, [THIRD_OF_FIVE_ARGS]
  mov [SECOND_VAR], eax

.loop:
  mov eax, [FIRST_VAR]          ; copy current position
  mov [THIRD_VAR], eax
  mov eax, [SECOND_VAR]
  mov [FOURTH_VAR], eax

  lodsb

  cmp al, 0x0
  je .end
  cmp al, 0xa
  je .press_button
  cmp al, 'L'
  je .left
  cmp al, 'R'
  je .right
  cmp al, 'U'
  je .up
  cmp al, 'D'
  je .down

  push unknown_direction_error
  push unknown_direction_error_len
  call print_error_and_exit

.press_button:
  push DWORD [FIRST_VAR]        ; add value of current button to output
  push DWORD [SECOND_VAR]
  push DWORD [FOURTH_OF_FIVE_ARGS]
  push DWORD [FIFTH_OF_FIVE_ARGS]
  call current_button
  add esp, 5*4

  stosb
  jmp .next

.left:
  dec DWORD [THIRD_VAR]
  jmp .move_if_possible
.right:
  inc DWORD [THIRD_VAR]
  jmp .move_if_possible
.up:
  dec DWORD [FOURTH_VAR]
  jmp .move_if_possible
.down:
  inc DWORD [FOURTH_VAR]

.move_if_possible:
  push DWORD [THIRD_VAR]        ; test if we're still above a button
  push DWORD [FOURTH_VAR]
  push DWORD [FOURTH_OF_FIVE_ARGS]
  push DWORD [FIFTH_OF_FIVE_ARGS]
  call current_button
  cmp al, 0x0                   ; if 0x0, then we're not.
  je .next

  mov eax, [THIRD_VAR]          ; current position = next position
  mov [FIRST_VAR], eax
  mov eax, [FOURTH_VAR]
  mov [SECOND_VAR], eax

.next:
  jmp .loop
.end:
  mov al, 0xa
  stosb
  mov al, 0x0
  stosb

  mov eax, edi
  sub eax, output_buffer

  mov esp, ebp
  pop ebp
  ret


_start:
  push ebp
  mov ebp, esp

  push input_file               ; read input file
  push input_buffer
  push input_bufsize
  call read_file
  add esp, 3*4
  mov [FIRST_VAR], eax

  push input_buffer
  push part1_keypad_start_x
  push part1_keypad_start_y
  push part1_keypad
  push part1_keypad_width
  call day2_part1
  add esp, 5*4

  push output_buffer
  push eax
  call write_stdout

  push input_buffer
  push part2_keypad_start_x
  push part2_keypad_start_y
  push part2_keypad
  push part2_keypad_width
  call day2_part1
  add esp, 5*4

  push output_buffer
  push eax
  call write_stdout

  mov eax, 0
  push eax
  call sys_exit
  ; no ret
