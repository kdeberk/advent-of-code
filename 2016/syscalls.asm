%ifndef __SYSCALLS__
%define __SYSCALLS__

%include "constants.asm"


;; System calls
SYS_EXIT  equ 1
SYS_OPEN  equ 5
SYS_READ  equ 3
SYS_WRITE equ 4

;; File open modifiers
READ_ONLY equ 0

;; File descriptors
STDIN     equ 0
STDOUT    equ 1
STDERR    equ 2


section .data
  sys_open_error db 'Input file could not be opened, does it exist?',0xa,0x0
  sys_open_error_len equ $ - sys_open_error ; TODO calculate later during runtime?
  sys_read_error db 'Opened file could not be read.',0xa,0x0
  sys_read_error_len equ $ - sys_read_error


section .text

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

%endif
