%include "constants.asm"
%include "io.asm"
%include "integer_utils.asm"
%include "string_utils.asm"
%include "md5.asm"

section .bss
  answer1 resb 8
  answer1_len equ $ - answer1
  answer2 resb 8
  answer2_len equ $ - answer2
  plaintext resb 32
  plaintext_len equ $ - plaintext
  hash resb 16                  ; 128 bits

section .data
  input db "cxdnnyjw"
  input_len equ $ - input
  integer times 16 db 0x0
  integer_len equ $ - integer
  hexchars db "0123456789abcdef"

section .text
  global _start

;; args: none
;; vars:
;; - current integer
;; - current location in answer1
;; - found letters for answer2
day5:
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
  mov DWORD [SECOND_VAR], answer1
  mov DWORD [THIRD_VAR], 0
.main_loop:
  push DWORD [FIRST_VAR]        ; converting i to string
  push hash
  call generate_hash
  add esp, 2*4

  mov eax, [hash]               ; compare first 5 bytes
  and eax, 0x00f0ffff
  cmp eax, 0x0
  jne .next

  mov eax, [SECOND_VAR]         ; check if first part is done
  sub eax, answer1
  cmp eax, answer1_len
  jne .first

  mov eax, [THIRD_VAR]          ; check if second part is done
  cmp eax, answer2_len
  jne .second
  jmp .done

.first:
  mov eax, [hash]               ; get 6th byte
  and eax, 0x000f0000
  shr eax, 16
  add eax, hexchars
  mov al, [eax]

  mov edi, [SECOND_VAR]         ; write hexchar
  stosb
  mov [SECOND_VAR], edi

.second:
  mov eax, [hash]               ; get 6th byte
  and eax, 0x000f0000
  shr eax, 16
  cmp eax, answer2_len
  jge .next

  mov edi, eax
  add edi, answer2

  mov eax, [edi]
  cmp al, 0x0
  jne .next

  mov eax, [hash]               ; get 7th byte
  and eax, 0xf0000000
  shr eax, 28
  add eax, hexchars
  mov al, [eax]
  stosb

  mov eax, [THIRD_VAR]
  add eax, 1
  mov [THIRD_VAR], eax

.next:
  add DWORD [FIRST_VAR], 1
  jmp .main_loop

.done:
  push answer1
  push answer1_len
  call write_stdout_append_newline
  add esp, 2*4

  push answer2
  push answer2_len
  call write_stdout_append_newline
  add esp, 2*4

  pop edx
  pop ecx
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - i
;; - hash location
;; returns: nothing
generate_hash:
  push ebp
  mov ebp, esp

  push DWORD [FIRST_OF_TWO_ARGS]
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
  add ecx, 1
  cmp esi, ebx
  jne .copy_loop

  push plaintext
  push ecx
  push hash
  call md5_hash
  add esp, 3*4

  mov esp, ebp
  pop ebp
  ret


_start:
  push ebp
  mov ebp, esp

  call day5

  push DWORD 0
  call sys_exit
