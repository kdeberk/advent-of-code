%ifndef __IO__
%define __IO__

%include "constants.asm"
%include "integer_utils.asm"
%include "string_utils.asm"
%include "syscalls.asm"

;; File open modifiers
READ_ONLY equ 0

;; File descriptors
STDIN     equ 0
STDOUT    equ 1
STDERR    equ 2


section .bss
  input_buffer resb 65536
  input_bufsize equ $ - input_buffer
  output_buffer resb 1024
  output_bufsize equ $ - output_buffer


section .data
  sys_open_error db 'Input file could not be opened, does it exist?',0xa,0x0
  sys_open_error_len equ $ - sys_open_error
  sys_read_error db 'Opened file could not be read.',0xa,0x0
  sys_read_error_len equ $ - sys_read_error
  sys_close_error db 'Could not close file.',0xa,0x0
  sys_close_error_len equ $ - sys_close_error


section .text

;; args:
;; - string,
;; - string length
;; returns: n bytes written
write_stderr:
  push ebp
  mov ebp, esp

  push STDERR
  push DWORD [FIRST_OF_TWO_ARGS]
  push DWORD [SECOND_OF_TWO_ARGS]
  call sys_write
  add esp, 3*4

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

  mov eax, [FIRST_OF_TWO_ARGS]
  push eax
  mov eax, [SECOND_OF_TWO_ARGS]
  push eax
  call strlen_if_not_specified
  add esp, 2*4

.write:
  push STDOUT
  push DWORD [FIRST_OF_TWO_ARGS]
  push DWORD eax
  call sys_write
  add esp, 3*4

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - zero terminated filename
;; - destination buffer
;; - buffer size
;; vars:
;; - file descriptor
;; - file size
;; returns:
;; - eax: n bytes written to buffer
read_file:
  push ebp
  mov ebp, esp

  sub esp, 2*4

  push DWORD [FIRST_OF_THREE_ARGS]
  push READ_ONLY
  call sys_open
  add esp, 2*4
  cmp eax, 0
  jl .open_failed
  mov [FIRST_VAR], eax

  push eax
  push DWORD [SECOND_OF_THREE_ARGS]
  push DWORD [THIRD_OF_THREE_ARGS]
  call sys_read
  add esp, 3*4
  cmp eax, 0
  jl .read_failed
  mov [SECOND_VAR], eax

  add eax, [SECOND_OF_THREE_ARGS] ; add 0x0 byte
  mov [eax], BYTE 0x0

  push DWORD [FIRST_VAR]
  call sys_close
  add esp, 1*4

  mov eax, [SECOND_VAR]
  jmp .return

.open_failed:
  push DWORD sys_open_error
  push DWORD sys_open_error_len
  call print_error_and_exit
  ; no ret

 .read_failed:
  push DWORD sys_read_error
  push DWORD sys_read_error_len
  call print_error_and_exit
  ; no ret

.return:
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

  push STDERR
  push DWORD [FIRST_OF_TWO_ARGS]
  push DWORD [SECOND_OF_TWO_ARGS]
  call sys_write
  add esp, 3*4

  push 1
  call sys_exit
  ; no ret


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


%endif
