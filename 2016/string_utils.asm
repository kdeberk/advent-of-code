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


;; args:
;; - esi: source string
;; - edi: destination string
;; - character to read until (but include copy)
;; returns: nothing
copy_string_until:
  cld

  push ebp
  mov ebp, esp

.loop:
  lodsb
  cmp al, [SINGLE_ARG]
  je .end
  stosb
  jmp .loop

.end:
  stosb

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - esi: source string
;; - character to read until
;; returns: nothing
skip_chars_until:
  cld

  push ebp
  mov ebp, esp

.loop:
  lodsb
  cmp al, 0x0
  je .end
  cmp al, [SINGLE_ARG]
  je .end
  jmp .loop

.end:
  mov esp, ebp
  pop ebp
  ret


;; args:
;; - esi: source string
;; returns:
;; - esi: location of first digit
skip_chars_until_digits:
  cld

  push ebp
  mov ebp, esp

.loop:
  lodsb
  cmp al, 0x0
  je .end
  cmp al, '0'
  jl .next
  cmp al, '9'
  jle .end
.next:
  jmp .loop

.end:
  dec esi

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - first string
;; - second string
;; - length to check
;; vars:
;; - current position first string
;; - current position second string
;; returns:
;; - eax: TRUE if strings match, FALSE if not
strcmp_len:
  push ebp
  mov ebp, esp

  push ecx
  push esi
  push edi

  mov esi, [FIRST_OF_THREE_ARGS]
  mov edi, [SECOND_OF_THREE_ARGS]
  mov ecx, [THIRD_OF_THREE_ARGS]

.loop:
  mov al, [esi]
  sub BYTE al, [edi]
  cmp al, 0
  jne .not_equal

  inc esi
  inc edi
  loop .loop

  mov eax, TRUE
  jmp .return
.not_equal:
  mov eax, FALSE
.return:
  pop edi
  pop esi
  pop ecx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - zero terminated string
;; - char to find
;; returns:
;; - eax: index of char, -1 if not found
str_index_of_char:
  push ebp
  mov ebp, esp

  push esi

  mov esi, [FIRST_OF_TWO_ARGS]
.loop:
  lodsb
  cmp al, [SECOND_OF_TWO_ARGS]
  je .found
  jmp .loop
  mov eax, -1
  jmp .return

.found:
  mov eax, esi
  sub eax, [FIRST_OF_TWO_ARGS]
  dec eax                       ; we read one char too much
.return:
  pop esi

  mov esp, ebp
  pop ebp
  ret


%endif
