%include "constants.asm"

section .bss
  lookup_table resb 26
  lookup_table_len equ $ - lookup_table


section .text

;; args:
;; - string to shift
;; - string length
;; - destination
;; - shift offset
;; returns:
;; - 3nd param: zero terminated encrypted string
shift_string:
  push ebp
  mov ebp, esp

  push ecx
  push esi
  push edi

  mov ecx, [SECOND_OF_FOUR_ARGS]
  mov esi, [FIRST_OF_FOUR_ARGS]
  mov edi, [THIRD_OF_FOUR_ARGS]

  push DWORD [FOURTH_OF_FOUR_ARGS]
  call _prepare_lookup_table
  add esp, 1*4

.loop:
  xor eax, eax

  lodsb
  cmp eax, 'a'
  jl .next

  sub eax, 'a'
  add eax, lookup_table
  mov al, [eax]                 ; c' = lookup[c - 'a']
.next:
  stosb
  loop .loop

  pop edi
  pop esi
  pop ecx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - offset
;; returns: nothing
_prepare_lookup_table:
  cld

  push ebp
  mov ebp, esp

  push eax
  push ebx
  push ecx
  push edx
  push edi

  mov ebx, lookup_table_len
  mov ecx, lookup_table_len
  mov edi, lookup_table
.loop:
  xor edx, edx

  mov eax, lookup_table_len
  sub eax, ecx
  add eax, [SINGLE_ARG]
  div ebx
  mov eax, edx
  add eax, 'a'
  stosb
  loop .loop

  pop edi
  pop edx
  pop ecx
  pop ebx
  pop eax

  mov esp, ebp
  pop ebp
  ret
