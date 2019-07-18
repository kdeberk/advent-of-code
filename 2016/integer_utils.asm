%ifndef __INTEGER_UTILS__
%define __INTEGER_UTILS__

%include "constants.asm"
%include "syscalls.asm"

DIV_10_CONSTANT equ 0xCCCCCCCD


section .text
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

%endif
