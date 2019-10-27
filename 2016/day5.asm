%include "constants.asm"
%include "io.asm"
%include "integer_utils.asm"
%include "string_utils.asm"
%include "md5.asm"

section .bss
  answer resb 9
  answer_len equ $ - answer
  plaintext resb 32
  plaintext_len equ $ - plaintext
  hash resb 16                  ; 128 bits

section .data
  input db "cxdnnyjw"
  input_len equ $ - input
  integer resb 16
  integer_len equ $ - integer
  hexchars db "0123456789abcdef"

section .text
  global _start

;; args: none
;; vars:
;; - current integer
;; returns:
;; - answer: containing the answer
day5_part1:
  push ebp
  mov ebp, esp

  sub esp, 2*4

  push ebx
  push ecx
  push edx

  push input                    ; copy prefix
  push plaintext
  push input_len
  call strcpy_len
  add esp, 3*4

  mov DWORD [FIRST_VAR], 1
  mov DWORD [SECOND_VAR], answer
.main_loop:
  push DWORD [FIRST_VAR]        ; converting i to string
  push integer
  push integer_len
  call integer_to_string
  add esp, 3*4

  mov esi, eax                  ; copying integer_str to plaintext
  mov edi, plaintext
  add edi, input_len
  mov ebx, integer
  add ebx, integer_len
  mov ecx, input_len
.copy_loop:
  lodsb
  stosb
  inc ecx
  cmp esi, ebx
  jne .copy_loop

  push plaintext
  push ecx
  push hash
  call md5_hash
  add esp, 3*4

  mov eax, [hash]               ; compare first 5 bytes
  and eax, 0x00f0ffff
  cmp eax, 0x0
  jne .next

  mov eax, [hash]               ; get 6th byte
  and eax, 0x000f0000
  shr eax, 16
  add eax, hexchars
  mov al, [eax]

  mov edi, [SECOND_VAR]         ; write hexchar
  stosb
  mov [SECOND_VAR], edi

  mov eax, edi
  sub eax, answer               ; check if 8 chars were found
  cmp eax, 8

  mov eax, 0xa                  ; add newline
  stosb

  je .done

.next:
  inc DWORD [FIRST_VAR]
  jmp .main_loop

.done:
  
  push answer
  push answer_len
  call write_stdout
  add esp, 4*2

  pop edx
  pop ecx
  pop ebx

  mov esp, ebp
  pop ebp
  ret


_start:
  push ebp
  mov ebp, esp

  call day5_part1

  push DWORD 0
  call sys_exit
