%ifndef __SYSCALLS__
%define __SYSCALLS__

%include "constants.asm"

;; System calls
SYS_CLOSE equ 6
SYS_EXIT  equ 1
SYS_OPEN  equ 5
SYS_READ  equ 3
SYS_WRITE equ 4


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
;; - file descriptor
;; returns:
;; - error code
sys_close:
  push ebp
  mov ebp, esp

  push SYS_CLOSE
  push DWORD [SINGLE_ARG]
  call sys_call_2

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
;; - file descriptor
;; - chars
;; - number of chars
sys_write:
  push ebp
  mov ebp, esp

  push SYS_WRITE
  push DWORD [FIRST_OF_THREE_ARGS]
  push DWORD [SECOND_OF_THREE_ARGS]
  push DWORD [THIRD_OF_THREE_ARGS]
  call sys_call_4
  add esp, 4*4

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - zero terminated filename
;; - file mode
;; returns on success:
;; - eax: file descriptor
sys_open:
  push ebp
  mov ebp, esp

  push SYS_OPEN
  push DWORD [FIRST_OF_TWO_ARGS]
  push DWORD [SECOND_OF_TWO_ARGS]
  call sys_call_3
  add esp, 3*4

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - file descriptor
;; - destination buffer
;; - buffer size
;; returns on success:
;; - eax: n bytes read, -1 if failed
sys_read:
  push ebp
  mov ebp, esp

  push SYS_READ
  push DWORD [FIRST_OF_THREE_ARGS]
  push DWORD [SECOND_OF_THREE_ARGS]
  push DWORD [THIRD_OF_THREE_ARGS]
  call sys_call_4
  add esp, 4*4

  mov esp, ebp
  pop ebp
  ret


%endif
