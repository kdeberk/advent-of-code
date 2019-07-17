;; System calls
SYS_EXIT  equ 1
SYS_OPEN  equ 5
SYS_READ  equ 3
SYS_WRITE equ 4

;; Basic base pointer helpers
%define SINGLE_ARG (ebp+8)

%define FIRST_OF_TWO_ARGS (ebp+12)
%define SECOND_OF_TWO_ARGS (ebp+8)

%define FIRST_OF_THREE_ARGS (ebp+16)
%define SECOND_OF_THREE_ARGS (ebp+12)
%define THIRD_OF_THREE_ARGS (ebp+8)

%define FIRST_OF_FOUR_ARGS (ebp+20)
%define SECOND_OF_FOUR_ARGS (ebp+16)
%define THIRD_OF_FOUR_ARGS (ebp+12)
%define FOURTH_OF_FOUR_ARGS (ebp+8)

%define FIRST_VAR (ebp-4)
%define SECOND_VAR (ebp-8)
%define THIRD_VAR (ebp-12)
%define FOURTH_VAR (ebp-16)

;; File open modifiers
READ_ONLY equ 0

;; File descriptors
STDIN     equ 0
STDOUT    equ 1
STDERR    equ 2

;; Cardinal directions
NORTH     equ 0
SOUTH     equ 1
WEST      equ 2
EAST      equ 3

DIV_10_CONSTANT equ 0xCCCCCCCD
GRID_WIDTH equ 500


section .bss
  input_buffer resb 1024
  input_bufsize equ $ - input_buffer
  output_buffer resb 1024
  output_bufsize equ $ - output_buffer
  grid resb GRID_WIDTH * GRID_WIDTH

section .data
  input_file db 'data/day1.txt',0x0
  sys_open_error db 'Input file could not be opened, does it exist?',0xa,0x0
  sys_open_error_len equ $ - sys_open_error ; TODO calculate later during runtime?
  sys_read_error db 'Opened file could not be read.',0xa,0x0
  sys_read_error_len equ $ - sys_read_error
  unknown_direction_error db 'Unknown direction error.',0xa,0x0
  unknown_direction_error_len equ $ - unknown_direction_error

  left_table db WEST, EAST, SOUTH, NORTH
  right_table db EAST, WEST, NORTH, SOUTH
  direction_x_table dd 0, 0, 1, -1
  direction_y_table dd 1, -1, 0, 0

section .text
  global _start


kernel:
  int	0x80
  ret

;; args: 2 items on the stack
;; returns:
;; - eax: result value of syscall
sys_call_2:
  push ebp
  mov ebp, esp

  push ebx

  mov eax, [FIRST_OF_TWO_ARGS]
  mov ebx, [SECOND_OF_TWO_ARGS]
  call kernel

  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args: 3 items on the stack
;; returns:
;; - eax: result value of syscall
sys_call_3:
  push ebp
  mov ebp, esp

  push ebx
  push ecx

  mov eax, [FIRST_OF_THREE_ARGS]
  mov ebx, [SECOND_OF_THREE_ARGS]
  mov ecx, [THIRD_OF_THREE_ARGS]
  call kernel

  pop ecx
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args: 4 items on the stack
;; returns:
;; - eax: result value of syscall
sys_call_4:
  push ebp
  mov ebp, esp

  push ebx
  push ecx
  push edx

  mov eax, [FIRST_OF_FOUR_ARGS]
  mov ebx, [SECOND_OF_FOUR_ARGS]
  mov ecx, [THIRD_OF_FOUR_ARGS]
  mov edx, [FOURTH_OF_FOUR_ARGS]
  call kernel

  pop edx
  pop ecx
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - exitcode
;; does not return
sys_exit:
  push ebp
  mov ebp, esp

  push SYS_EXIT
  push DWORD [SINGLE_ARG]
  call sys_call_2
  ; no ret


;; args:
;; - string,
;; - string length
;; returns: n bytes written
write_stderr:
  push ebp
  mov ebp, esp

  push SYS_WRITE
  push STDERR
  push DWORD [FIRST_OF_TWO_ARGS]
  push DWORD [SECOND_OF_TWO_ARGS]
  call sys_call_4
  add esp, 4*4

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - string
;; - string length
;; returns: n bytes written
write_stdout:
  push ebp
  mov ebp, esp

  push SYS_WRITE
  push STDOUT
  push DWORD [FIRST_OF_TWO_ARGS]
  push DWORD [SECOND_OF_TWO_ARGS]
  call sys_call_4
  add esp, 4*4

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - string
;; - string length
;; does not return
print_error_and_exit:
  push ebp
  mov ebp, esp

  push DWORD [FIRST_OF_TWO_ARGS]
  push DWORD [SECOND_OF_TWO_ARGS]
  call write_stderr
  add esp, 2*4

  push 1
  call sys_exit
  ; no ret


;; args:
;; - zero terminated filename
;; returns on success:
;; - eax: file descriptor
sys_open_read_only:
  push ebp
  mov ebp, esp

  push SYS_OPEN
  push DWORD [SINGLE_ARG]
  push READ_ONLY
  call sys_call_3
  add esp, 3*4

  cmp eax, 0
  jge .success

  push DWORD sys_open_error
  push DWORD sys_open_error_len
  call print_error_and_exit
  add esp, 2*4
.success:
  mov esp, ebp
  pop ebp
  ret


;; args:
;; - file descriptor
;; - destination buffer
;; - buffer size
;; returns on success:
;; - eax: n bytes read
sys_read:
  push ebp
  mov ebp, esp

  push SYS_READ
  push DWORD [FIRST_OF_THREE_ARGS]
  push DWORD [SECOND_OF_THREE_ARGS]
  push DWORD [THIRD_OF_THREE_ARGS]
  call sys_call_4
  add esp, 4*4

  cmp eax, 0
  jge .success

  push DWORD sys_read_error
  push DWORD sys_read_error_len
  call print_error_and_exit
  add esp, 2*4
.success:
  mov esp, ebp
  pop ebp
  ret


;; args:
;; - zero terminated filename
;; - destination buffer
;; - buffer size
;; vars:
;; - filesize
;; returns:
;; - eax: n bytes written to buffer
open_file_and_read:
  push ebp
  mov ebp, esp

  sub esp, 1*4

  push DWORD [FIRST_OF_THREE_ARGS]
  call sys_open_read_only
  add esp, 1*4

  push eax
  push DWORD [SECOND_OF_THREE_ARGS]
  push DWORD [THIRD_OF_THREE_ARGS]
  call sys_read
  add esp, 3*4
  mov [FIRST_VAR], eax

  add eax, [SECOND_OF_THREE_ARGS]
  mov [eax], BYTE 0x0

  mov eax, [FIRST_VAR]

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - integer
;; returns:
;; - eax: absolute value
integer_abs:
  push ebp
  mov ebp, esp

  push ecx

  mov eax, [SINGLE_ARG]
  mov ecx, eax
  neg eax
  cmovl eax, ecx                ; if eax is negative, replace with positive

  pop ecx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - integer
;; - output buffer
;; - output buffer size
;; returns:
;; - eax: location in output buffer
;; - ebx: byte length
integer_to_string:
  std                           ; decreasing edi for every stosb

  push ebp
  mov ebp, esp

  push ebx
  push ecx
  push edx

  mov ebx, DIV_10_CONSTANT
  mov edi, [SECOND_OF_THREE_ARGS]
  add edi, [THIRD_OF_THREE_ARGS]

  push DWORD [FIRST_OF_THREE_ARGS]
  call integer_abs
  add esp, 1*4
.loop:
  mov ecx, eax

  mul ebx
  shr edx, 3

  mov eax, edx

  lea edx, [edx*4 + edx]
  lea edx, [edx*2 - '0']
  sub ecx, edx

  push eax
  mov eax, ecx
  stosb
  pop eax

  test eax, eax
  jnz .loop
.end:
  mov eax, [FIRST_OF_THREE_ARGS] ; print '-' if input was negative
  cmp eax, 0x0
  jge .no_sign

  mov al, '-'
  stosb
.no_sign:
  mov eax, edi
  inc eax

  pop edx
  pop ecx
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - integer
;; vars:
;; - start of integer
;; - string char size, including 0xa
;; returns:
;; - eax: bytes written
print_integer:
  push ebp
  mov ebp, esp

  sub esp, 2*4

  mov eax, output_buffer
  add eax, output_bufsize
  dec eax
  mov BYTE [eax], 0xa

  push DWORD [SINGLE_ARG]
  push output_buffer
  mov eax, output_bufsize
  sub eax, 2
  push eax
  call integer_to_string
  add esp, 3*4

  mov [FIRST_VAR], eax
  mov eax, output_buffer
  add eax, output_bufsize
  sub eax, [FIRST_VAR]

  mov [SECOND_VAR], eax

  push DWORD [FIRST_VAR]
  push DWORD [SECOND_VAR]
  call write_stdout
  add esp, 2*4

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - zero-terminated string
;; - char to count
;; registers:
;; - esi: an iterator over the string
;; - ebx: the char being counted
;; - eax: the count
;; returns:
;; - eax: count of char
count_specific_char:
  cld

  push ebp
  mov ebp, esp

  push ebx
  push ecx
  push esi

  mov esi, [FIRST_OF_TWO_ARGS]
  mov ebx, [SECOND_OF_TWO_ARGS]
  mov ecx, 0
.loop:
  lodsb
  cmp al, 0x0                   ; test end-of-string
  je .end

  cmp al, bl
  jne .next
  inc ecx                       ; increase count
.next:
  jmp .loop
.end:
  mov eax, ecx

  pop esi
  pop ecx
  pop ebx

  mov esp, ebp
  pop ebp
  ret

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

  push input_file               ; open file
  push input_buffer
  push input_bufsize
  call open_file_and_read
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
