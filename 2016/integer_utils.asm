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
;; N.B. doesn't support 0
integer_to_string:
  std                           ; decreasing edi for every stosb

  push ebp
  mov ebp, esp

  push ebx
  push ecx
  push edx
  push edi

  mov ebx, DIV_10_CONSTANT
  mov edi, [SECOND_OF_THREE_ARGS]
  add edi, [THIRD_OF_THREE_ARGS]
  sub edi, 1

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

  pop edi
  pop edx
  pop ecx
  pop ebx

  cld                           ; TODO: pop original value of DF

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - esi: zero-terminated string
;; vars:
;; - read integer
;; returns:
;; - eax: read integer
read_integer:
  cld

  push ebp
  mov ebp, esp

  sub esp, 1*4

  push ecx

  mov DWORD [FIRST_VAR], 0
  mov ecx, 0
.loop:
  mov eax, 0

  lodsb
  cmp al, 0
  je .end_of_stream

  sub al, '0'
  cmp al, 0
  jl .return_integer            ; expected non-digits are < '0'
  cmp al, 9
  jg .return_integer            ; expected non-digits are > '9'

  mov cl, al                    ; distance += 10*distance + cl
  mov eax, [FIRST_VAR]
  imul eax, 10
  add eax, ecx
  mov DWORD [FIRST_VAR], eax

  jmp .loop
.end_of_stream:
  mov eax, 0
  dec esi                       ; we read one byte too many
  jmp .return
.return_integer:
  mov eax, [FIRST_VAR]
.return:
  pop ecx

  mov esp, ebp
  pop ebp
  ret

%endif
