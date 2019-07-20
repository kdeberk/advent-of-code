%ifndef __STRING_UTILS__
%define __STRING_UTILS__

  
%include "constants.asm"


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
  cmp al, 0x0                   ; test end-of-string
  je .end

  cmp al, bl
  jne .next
  inc ecx                       ; increase count
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
;; - esi: zero terminated string
;; returns:
;; - esi: string after reading
read_whitespace:
  cld

  push ebp
  mov ebp, esp

.loop:
  lodsb

  cmp al, 0x0
  je .end

  cmp al, '0'                   ; all whitespace chars are < '0'
  jge .toofar

  jmp .loop
.toofar:
  dec esi
.end:
  mov esp, ebp
  pop ebp
  ret


%endif
