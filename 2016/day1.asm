%include "constants.asm"
%include "syscalls.asm"
%include "integer_utils.asm"
%include "string_utils.asm"


;; Cardinal directions
NORTH     equ 0
SOUTH     equ 1
WEST      equ 2
EAST      equ 3

GRID_WIDTH equ 500

section .bss
  input_buffer resb 1024
  input_bufsize equ $ - input_buffer
  output_buffer resb 1024
  output_bufsize equ $ - output_buffer
  grid resb GRID_WIDTH * GRID_WIDTH

section .data
  input_file db 'data/day1.txt',0x0
  unknown_direction_error db 'Unknown direction error.',0xa,0x0
  unknown_direction_error_len equ $ - unknown_direction_error

  left_table db WEST, EAST, SOUTH, NORTH
  right_table db EAST, WEST, NORTH, SOUTH
  direction_x_table dd 0, 0, 1, -1
  direction_y_table dd 1, -1, 0, 0

section .text
  global _start


;; args:
;; - x position
;; - y position
;; vars:
;; - sum
;; returns:
;; - eax: abs(x)+abs(y)
distance_to_origin:
  push ebp
  mov ebp, esp

  sub esp, 1*4

  push DWORD [FIRST_OF_TWO_ARGS]
  call integer_abs
  add esp, 1*4
  mov [FIRST_VAR], eax

  push DWORD [SECOND_OF_TWO_ARGS]
  call integer_abs
  add esp, 1*4
  add eax, [FIRST_VAR]

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - esi: zero-terminated string
;; - current direction
;; returns:
;; - eax: updated direction
;; - esi: string after reading
read_direction:
  cld

  push ebp
  mov ebp, esp

  push ebx

  mov eax, 0
  mov ebx, [SINGLE_ARG]

  lodsb
  cmp al, 'L'
  je .left
  cmp al, 'R'
  je .right

  push DWORD unknown_direction_error
  push DWORD unknown_direction_error_len
  call print_error_and_exit
  ; unreachable
.left:
  mov al, BYTE [left_table+ebx]
  jmp .return
.right:
  mov al, BYTE [right_table+ebx]
.return:
  pop ebx

  mov esp, ebp
  pop ebp
  ret

;; args:
;; - esi: zero-terminated string
;; registers:
;; - eax: to return read value
;; vars:
;; - read integer
;; returns:
;; - eax: distance
read_distance:
  cld

  push ebp
  mov ebp, esp

  sub esp, 1*4

  push ecx

  mov DWORD [FIRST_VAR], 0
  mov ecx, 0
.loop:
  lodsb
  sub al, '0'
  cmp al, 0
  jl .end                       ; expected non-digits are < '0'

  mov cl, al                    ; distance += 10*distance + cl
  mov eax, [FIRST_VAR]
  imul eax, 10
  add eax, ecx
  mov DWORD [FIRST_VAR], eax

  jmp .loop
.end:
  mov eax, [FIRST_VAR]

  pop ecx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - esi: zero terminated string
;; returns:
;; - esi: string after reading
read_whitespace:
  cld

  push ebp
  mov ebp, esp

.loop:
  lodsb
  cmp al, 0x0
  jmp .end

  cmp al, '0'                   ; all whitespace chars are < '0'
  jge .toofar

  jmp .loop
.toofar:
  dec esi
.end:
  mov esp, ebp
  pop ebp
  ret


;; args:
;; - zero-terminated string
;; - n items
;; vars:
;; - direction
;; - x value
;; - y value
;; - last read distance
;; returns:
;; - eax: distance of final place to origin
day1_part1:
  push ebp
  mov ebp, esp

  sub esp, 4*4
  push ebx
  push ecx
  push edx

  mov DWORD [FIRST_VAR], NORTH
  mov DWORD [SECOND_VAR], 0
  mov DWORD [THIRD_VAR], 0

  mov esi, [FIRST_OF_TWO_ARGS]
  mov ecx, [SECOND_OF_TWO_ARGS]

.loop:
  push DWORD [FIRST_VAR]        ; read direction
  call read_direction
  add esp, 1*4
  mov [FIRST_VAR], eax

  call read_distance            ; read distance
  mov [FOURTH_VAR], eax

  mov eax, [FIRST_VAR]          ; update x position
  mov eax, [direction_x_table+(eax*4)]
  mov ebx, [FOURTH_VAR]
  imul eax, ebx
  add eax, [SECOND_VAR]
  mov [SECOND_VAR], eax

  mov eax, [FIRST_VAR]          ; update y position
  mov eax, [direction_y_table+(eax*4)]
  mov ebx, [FOURTH_VAR]
  imul eax, ebx
  add eax, [THIRD_VAR]
  mov [THIRD_VAR], eax

  call read_whitespace          ; read until next entry

  loop .loop
.end:
  push DWORD [SECOND_VAR]
  push DWORD [THIRD_VAR]
  call distance_to_origin
  add esp, 2*4

  pop edx
  pop ecx
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - x position
;; - y position
;; returns:
;; - eax: memory location in grid
calculate_grid_location:
  push ebp
  mov ebp, esp

  mov eax, GRID_WIDTH / 2
  add eax, [FIRST_OF_TWO_ARGS]
  imul eax, GRID_WIDTH
  add eax, [SECOND_OF_TWO_ARGS]
  add eax, GRID_WIDTH / 2
  add eax, grid

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - direction
;; - x position
;; - y position
;; - distance
;; vars:
;; - x position
;; - y position
;; - memory location
;; returns:
;; - eax: distance to origin, if any
trace_path_in_grid:
  push ebp
  mov ebp, esp

  sub esp, 3*4

  push ecx

  mov eax, [SECOND_OF_FOUR_ARGS]
  mov [FIRST_VAR], eax
  mov eax, [THIRD_OF_FOUR_ARGS]
  mov [SECOND_VAR], eax
  mov ecx, [FOURTH_OF_FOUR_ARGS]
.loop:
  push DWORD [FIRST_VAR]        ; calculate byte in grid
  push DWORD [SECOND_VAR]
  call calculate_grid_location
  add esp, 2*4
  mov [THIRD_VAR], eax

  cmp BYTE [eax], 0             ; test if byte was previously set
  jne .found
  mov BYTE [eax], 0x1           ; set it now

  mov eax, [FIRST_OF_FOUR_ARGS] ; update x position
  mov eax, [direction_x_table+(eax*4)]
  add eax, [FIRST_VAR]
  mov [FIRST_VAR], eax

  mov eax, [FIRST_OF_FOUR_ARGS] ; update y position
  mov eax, [direction_y_table+(eax*4)]
  add eax, [SECOND_VAR]
  mov [SECOND_VAR], eax

  loop .loop

  mov eax, 0                    ; nothing found, return 0
  jmp .end
.found:
  push DWORD [FIRST_VAR]        ; result found, return distance to origin
  push DWORD [SECOND_VAR]
  call distance_to_origin
  add esp, 2*4
.end:
  pop ecx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - zero terminated string
;; - n items
;; vars:
;; - direction
;; - x value
;; - y value
;; - last read distance
;; returns:
;; - eax: distance of first twice visited place
day1_part2:
  push ebp
  mov ebp, esp

  sub esp, 4*4
  push ebx
  push ecx
  push edx

  mov DWORD [FIRST_VAR], NORTH
  mov DWORD [SECOND_VAR], 0
  mov DWORD [THIRD_VAR], 0

  mov esi, [FIRST_OF_TWO_ARGS]
  mov ecx, [SECOND_OF_TWO_ARGS]

.loop:
  push DWORD [FIRST_VAR]        ; read direction
  call read_direction
  add esp, 1*4
  mov [FIRST_VAR], eax

  call read_distance            ; read distance
  mov [FOURTH_VAR], eax

  push DWORD [FIRST_VAR]        ; trace path in grid
  push DWORD [SECOND_VAR]
  push DWORD [THIRD_VAR]
  push DWORD [FOURTH_VAR]
  call trace_path_in_grid
  add esp, 4*4

  cmp eax, 0                    ; test if value was returned
  jne .end

  mov eax, [FIRST_VAR]          ; update x position
  mov eax, [direction_x_table+(eax*4)]
  mov ebx, [FOURTH_VAR]
  imul eax, ebx
  add eax, [SECOND_VAR]
  mov [SECOND_VAR], eax

  mov eax, [FIRST_VAR]          ; update y position
  mov eax, [direction_y_table+(eax*4)]
  mov ebx, [FOURTH_VAR]
  imul eax, ebx
  add eax, [THIRD_VAR]
  mov [THIRD_VAR], eax

  call read_whitespace          ; read until next entry

  loop .loop
.end:
  pop edx
  pop ecx
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args: none
;; vars:
;; - size of input file
;; - item count
;; returns: nothing
_start:
  push ebp
  mov ebp, esp

  sub esp, 2*4

  push input_file               ; read input file
  push input_buffer
  push input_bufsize
  call read_file
  add esp, 3*4
  mov [FIRST_VAR], eax

  push input_buffer             ; count ','
  push ','
  call count_specific_char
  add esp, 2*4
  add eax, 1                    ; item count is ','-count + 1
  mov [SECOND_VAR], eax

  push input_buffer             ; part 1
  push DWORD [SECOND_VAR]
  call day1_part1
  add esp, 2*4

  push eax
  call print_integer
  add esp, 1*4

  push input_buffer             ; part 2
  push DWORD [SECOND_VAR]
  call day1_part2
  add esp, 2*4

  push eax
  call print_integer
  add esp, 1*4

  mov eax, 0
  push eax
  call sys_exit
  ; no ret
