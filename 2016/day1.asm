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

section .bss
  input_buffer resb 1024
  input_bufsize equ $ - input_buffer
  output_buffer resb 1024
  output_bufsize equ $ - output_buffer

section .data
  input_file db 'data/day1.txt',0x0
  sys_open_error db 'Input file could not be opened, does it exist?',0xa,0x0
  sys_open_error_len equ $ - sys_open_error ; TODO calculate later during runtime?
  sys_read_error db 'Opened file could not be read.',0xa0,0x0
  sys_read_error_len equ $ - sys_read_error
  unknown_direction_error db 'Unknown direction error.',0xa0,0x0
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
  cmovl eax, ecx

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
  std

  push ebp
  mov ebp, esp

  push ebx
  push ecx
  push edx

  mov ebx, 0xCCCCCCCD
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
  mov eax, [FIRST_OF_THREE_ARGS]
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
  cmp al, 0x0
  je .end

  cmp al, bl
  jne .next
  inc ecx
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
;; - zero-terminated string
;; - current cardinal direction
;; returns:
;; - eax: read direction
;; - ebx: string after reading
read_direction:
  push ebp
  mov ebp, esp

  mov ebx, [FIRST_OF_TWO_ARGS]
  mov eax, [SECOND_OF_TWO_ARGS]

  cmp BYTE [ebx], 'L'
  je .left
  cmp BYTE [ebx], 'R'
  je .right

  push DWORD unknown_direction_error
  push DWORD unknown_direction_error_len
  call print_error_and_exit
  ; unreachable
.left:
  mov al, BYTE [left_table+eax]
  jmp .return
.right:
  mov al, BYTE [right_table+eax]
.return:
  inc ebx

  mov esp, ebp
  pop ebp
  ret

;; args:
;; - zero-terminated string
;; registers:
;; - eax: to return read value
;; - ebx: to return updated bufsize
;; - ecx: stores the read character
;; vars:
;; - read integer
;; returns:
;; - eax: distance
;; - ebx: string after reading
read_distance:
  cld

  push ebp
  mov ebp, esp

  sub esp, 1*4

  push ecx
  push esi

  mov DWORD [FIRST_VAR], 0
  mov esi, [SINGLE_ARG]
  mov ecx, 0
.loop:
  lodsb
  sub al, '0'
  cmp al, 0
  jl .end

  mov cl, al
  mov eax, [FIRST_VAR]
  imul eax, 10
  add eax, ecx
  mov DWORD [FIRST_VAR], eax

  jmp .loop
.end:
  mov ebx, esi
  mov eax, [FIRST_VAR]

  pop esi
  pop ecx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - zero terminated string
;; returns:
;; - eax: string after reading
read_whitespace:
  push ebp
  mov ebp, esp

  mov eax, [SINGLE_ARG]

.loop:
  cmp BYTE [eax], ' '
  je .next
  cmp BYTE [eax], 0xa
  je .next
  cmp BYTE [eax], ','
  je .next
  jmp .end
.next:
  inc eax
  jmp .loop
.end:
  mov esp, ebp
  pop ebp
  ret


;; args:
;; - zero-terminated string
;; - n_items
;; vars:
;; - direction
;; - x value
;; - y value
;; - last read distance
;; returns:
;; - eax: sum of x and y value
day1_part1:
  push ebp
  mov ebp, esp

  sub esp, 4*4
  push ebx
  push edx

  mov DWORD [FIRST_VAR], NORTH
  mov DWORD [SECOND_VAR], 0
  mov DWORD [THIRD_VAR], 0

  mov ecx, [SECOND_OF_TWO_ARGS]
  mov edx, [FIRST_OF_TWO_ARGS]
.loop:
  push edx                      ; read direction
  push DWORD [FIRST_VAR]
  call read_direction
  add esp, 2*4
  mov edx, ebx
  mov [FIRST_VAR], eax

  push edx                      ; read distance
  call read_distance
  mov edx, ebx
  add esp, 1*4
  mov [FOURTH_VAR], eax

  mov eax, [FIRST_VAR]
  mov eax, [direction_x_table+(eax*4)]
  mov ebx, [FOURTH_VAR]
  imul eax, ebx
  add eax, [SECOND_VAR]
  mov [SECOND_VAR], eax

  mov eax, [FIRST_VAR]
  mov eax, [direction_y_table+(eax*4)]
  mov ebx, [FOURTH_VAR]
  imul eax, ebx
  add eax, [THIRD_VAR]
  mov [THIRD_VAR], eax

  push edx
  call read_whitespace
  mov edx, eax
  add esp, 1*4
.next:
  dec ecx
  cmp ecx, 0
  je .end
  jmp .loop
.end:
  pop edx
  pop ebx

  push DWORD [SECOND_VAR]
  call integer_abs
  mov DWORD [SECOND_VAR], eax
  push DWORD [THIRD_VAR]
  call integer_abs
  mov DWORD [THIRD_VAR], eax

  mov eax, [SECOND_VAR]
  add eax, [THIRD_VAR]

  mov esp, ebp
  pop ebp
  ret


;; args: none
;; vars: size of input file, item count
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

  ; push input_buffer             ; print contents
  ; push DWORD [FIRST_VAR]
  ; call write_stdout
  ; add esp, 2*4

  push input_buffer             ; count items
  push ','
  call count_specific_char
  add esp, 2*4
  add eax, 1
  mov [SECOND_VAR], eax

  push input_buffer
  push DWORD [SECOND_VAR]
  call day1_part1
  add esp, 2*4

  push eax
  call print_integer
  add esp, 1*4

  mov eax, 0
  push eax
  call sys_exit
  ; no ret
