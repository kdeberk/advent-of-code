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

%define FIRST_VAR (ebp-4)
%define SECOND_VAR (ebp-8)
%define THIRD_VAR (ebp-8)

ONE_REGISTER_PUSHED equ 4
TWO_REGISTERS_PUSHED equ 8

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


section .bss
  input_buffer resb 1024
  input_bufsize equ $ - input_buffer

section .data
  input_file db 'data/day1.txt',0x0
  sys_open_error db 'Input file could not be opened, does it exist?',0xa,0x0
  sys_open_error_len equ $ - sys_open_error ; TODO calculate later during runtime?
  sys_read_error db 'Opened file could not read.',0xa0,0x0
  sys_read_error_len equ $ - sys_read_error
  unknown_direction_error db 'Unknown direction error.',0xa0,0x0
  unknown_direction_error_len equ $ - unknown_direction_error

  left_table db WEST, EAST, SOUTH, NORTH
  right_table db EAST, WEST, NORTH, SOUTH

section .text
  global _start


;; args: none
;; vars: none
;; returns: nothing
kernel:
  int	0x80
  ret


;; args: exitcode
;; vars: none
;; returns: nothing
sys_exit:
  mov eax, SYS_EXIT
  mov ebx, [esp+4]
  call kernel
  ; no ret


;; args: string, length
;; vars: none
;; returns: n bytes written
write_stderr:
  push ebp
  mov ebp, esp

  mov eax, SYS_WRITE
  mov ebx, STDERR
  mov ecx, [FIRST_OF_TWO_ARGS]
  mov edx, [SECOND_OF_TWO_ARGS]
  call kernel

  mov esp, ebp
  pop ebp
  ret


;; args: string, length
;; vars: none
;; returns: n bytes written
write_stdout:
  push ebp
  mov ebp, esp

  mov eax, SYS_WRITE
  mov ebx, STDOUT
  mov ecx, [FIRST_OF_TWO_ARGS]
  mov edx, [SECOND_OF_TWO_ARGS]
  call kernel

  mov esp, ebp
  pop ebp
  ret


;; args: string, length
;; vars: none
;; returns: nothing
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


;; args: filename
;; vars: none
;; returns: file descriptor (eax) if success
sys_open_read_only:
  push ebp
  mov ebp, esp

  mov eax, SYS_OPEN
  mov ebx, [SINGLE_ARG]
  mov ecx, READ_ONLY
  call kernel

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


;; args: file descriptor, destination, n_bytes
;; vars: none
;; returns: n bytes read (eax) if success
sys_read:
  push ebp
  mov ebp, esp

  mov eax, SYS_READ
  mov ebx, [FIRST_OF_THREE_ARGS]
  mov ecx, [SECOND_OF_THREE_ARGS]
  mov edx, [THIRD_OF_THREE_ARGS]
  call kernel

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


;; args: filename, buffer, bufsize
;; vars: file descriptor, file size
;; returns: filesize
open_file_and_read:
  push ebp
  mov ebp, esp
  sub esp, 2*4

  push DWORD [FIRST_OF_THREE_ARGS]
  call sys_open_read_only
  add esp, 1*4
  mov [FIRST_VAR], eax

  push DWORD [FIRST_VAR]
  push DWORD [SECOND_OF_THREE_ARGS]
  push DWORD [THIRD_OF_THREE_ARGS]
  call sys_read
  add esp, 3*4
  mov [SECOND_VAR], eax

  mov esp, ebp
  pop ebp
  ret


;; args: zero-terminated string, char
;; vars: none
;; registers:
;; - edx: an iterator over the string
;; - ebx: the char being counted
;; - eax: the count
;; returns count of item
count_specific_char:
  push ebp
  mov ebp, esp

  mov edx, [FIRST_OF_TWO_ARGS]
  mov ebx, [SECOND_OF_TWO_ARGS]
  xor eax, eax
.loop:
  cmp BYTE [edx], 0x0
  je .end

  cmp BYTE [edx], bl
  jne .next
  inc eax
.next:
  inc edx
  jmp .loop
.end:
  mov esp, ebp
  pop ebp
  ret


;; args:
;; - buffer, a zero-terminated string, updated to reflect new value
;; - current cardinal direction
;; returns:
;; - eax: stores direction
;; - ebx: stores new bufsize
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
  mov eax, [left_table+eax]
  jmp .return
.right:
  mov eax, [right_table+eax]
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
;; returns distance
read_distance:
  push ecx
  push ebp
  mov ebp, esp

  xor ecx, ecx
  xor eax, eax
  mov ebx, [SINGLE_ARG+ONE_REGISTER_PUSHED]

.loop:
  cmp BYTE [ebx], 0x0           ; check end conditions
  je .end
  cmp BYTE [ebx], ','
  je .end

  imul eax, 10                  ; eax = eax*10 + (ecx-'0')
  mov BYTE cl, [ebx]
  sub ecx, '0'
  add eax, ecx
.next:
  inc ebx
  jmp .loop
.end:
  mov esp, ebp
  pop ebp
  pop ecx
  ret


read_whitespace:
  push ebp
  mov ebp, esp

  mov eax, [SINGLE_ARG]

.loop:
  cmp BYTE [eax], ' '
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


;; args: zero-terminated string, n_items
;; vars:
;; - direction
;; - x value
;; - y value
;; returns sum of x and y value
day1_part1:
  push ebp
  mov ebp, esp

  sub esp, 3*4
  mov DWORD [FIRST_VAR], NORTH
  mov DWORD [SECOND_VAR], 0
  mov DWORD [THIRD_VAR], 0

  mov edx, [FIRST_OF_TWO_ARGS]

.loop:
  push edx                      ; read direction, store in [FIRST_VAR]
  push DWORD [FIRST_VAR]
  call read_direction
  mov edx, ebx
  mov [FIRST_VAR], eax
  add esp, 2*4

  push edx                      ; read distance, keep in eax
  call read_distance
  mov edx, ebx
;; TODO: read distance
  add esp, 1*4

;; determine which axis will be modified, something that flips between 0 and 1?
;; determine modification 

  push edx
  call read_whitespace
  cmp eax, edx
  je .end                       ; we're at EOS

  mov edx, eax
  push eax
  call sys_exit
.next:
  jmp .loop
.end:
  ; mov eax, ecx
  ; add eax, edx

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

  push input_buffer             ; print contents
  push DWORD [FIRST_VAR]
  call write_stdout
  add esp, 2*4

  push input_buffer             ; count items
  push ','
  call count_specific_char
  add esp, 2*4
  add eax, 1
  mov [SECOND_VAR], eax

  push input_buffer
  push DWORD [SECOND_VAR]
  call day1_part1

  xor eax, eax
  push eax
  call sys_exit
  ; no ret
