%include "io.asm"

%define n_letters 26

section .bss
  counts resb n_letters
  answer1 resb 10
  answer2 resb 10

section .data
  input_file db 'data/day6.txt'

section .text
  global _start


;; args:
;; - string
;; - n of lines
;; - line width
day6_part1:
  push ebp
  mov ebp, esp

  sub esp, 1*4
  mov DWORD [FIRST_VAR], 0

  mov ecx, 0
  mov edi, answer1

.letter_loop:
  mov eax, [FIRST_OF_THREE_ARGS] ; count frequencies in column
  add eax, ecx
  push eax
  push DWORD [SECOND_OF_THREE_ARGS]
  mov eax, [THIRD_OF_THREE_ARGS]
  add eax, 1
  push eax
  call count_letter_column
  add esp, 3*4

  push DWORD counts             ; determine most frequent letter
  push DWORD n_letters
  call determine_most_frequent_letter
  add esp, 2*4

  mov edi, answer1
  mov [edi+ecx], al

  push DWORD counts             ; determine most frequent letter
  push DWORD n_letters
  call determine_least_frequent_letter
  add esp, 2*4

  mov edi, answer2
  mov [edi+ecx], al

  add ecx, 1
  cmp ecx, [THIRD_OF_THREE_ARGS]
  jne .letter_loop

  push DWORD answer1            ; print answer1
  mov eax, [THIRD_OF_THREE_ARGS]
  add eax, 1
  push eax
  call write_stdout_append_newline
  add esp, 2*4

  push DWORD answer2            ; print answer2
  mov eax, [THIRD_OF_THREE_ARGS]
  add eax, 1
  push eax
  call write_stdout_append_newline
  add esp, 2*4

  pop edi
  pop ecx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - string
;; - n lines
;; - line width
count_letter_column:
  push ebp
  mov ebp, esp

  push ecx
  push esi

  push counts                   ; memset counts to 0x0
  push DWORD 0x0
  push n_letters
  call memset
  add esp, 3*4

  mov ecx, [SECOND_OF_THREE_ARGS]
  mov esi, [FIRST_OF_THREE_ARGS]
.count_loop:
  xor eax, eax
  mov al, [esi]
  sub al, 'a'
  add eax, counts
  add BYTE [eax], 1

  add esi, [THIRD_OF_THREE_ARGS] ; go to next line
  sub ecx, 1
  cmp ecx, 0x0
  jne .count_loop 

  pop esi
  pop ecx

  mov esp, ebp
  pop ebp
  ret


;; args: none
;; returns:
;; - most frequent letter in $al
;; vars:
;; - location of best count
determine_most_frequent_letter:
  push ebp
  mov ebp, esp

  sub esp, 1*4

  push ebx
  push esi

  mov DWORD [FIRST_VAR], counts
  mov esi, counts
.loop:
  xor eax, eax
  lodsb
  mov ebx, [FIRST_VAR]
  mov bl, [ebx]
  sub bl, al
  jns .next
  mov [FIRST_VAR], esi
  sub DWORD [FIRST_VAR], 1

.next:
  mov eax, counts
  add eax, n_letters
  cmp eax, esi
  jne .loop

  mov eax, [FIRST_VAR]          ; convert index to letter
  sub eax, counts
  add eax, 'a'

  pop esi
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; args: none
;; returns:
;; - most frequent letter in $al
;; vars:
;; - location of best count
determine_least_frequent_letter:
  push ebp
  mov ebp, esp

  sub esp, 1*4

  push ebx
  push esi

  mov DWORD [FIRST_VAR], counts
  mov esi, counts
.loop:
  xor eax, eax
  lodsb
  mov ebx, [FIRST_VAR]
  mov bl, [ebx]
  sub al, bl
  jns .next
  mov [FIRST_VAR], esi
  sub DWORD [FIRST_VAR], 1

.next:
  mov eax, counts
  add eax, n_letters
  cmp eax, esi
  jne .loop

  mov eax, [FIRST_VAR]          ; convert index to letter
  sub eax, counts
  add eax, 'a'

  pop esi
  pop ebx

  mov esp, ebp
  pop ebp
  ret


;; vars:
;; - size of input file
;; - n of lines
;; - line width
_start:
  push ebp
  mov ebp, esp

  sub esp, 3*4

  push input_file               ; read file
  push input_buffer
  push input_bufsize
  call read_file
  add esp, 3*4
  mov [FIRST_VAR], eax

  push input_buffer             ; count line width
  push 0xa
  call str_index_of_char
  add esp, 2*4
  mov [THIRD_VAR], eax

  xor edx, edx                  ; determine n of lines
  mov ebx, eax
  add ebx, 1                    
  mov eax, [FIRST_VAR]
  div ebx
  mov [SECOND_VAR], eax

  push input_buffer
  push DWORD [SECOND_VAR]
  push DWORD [THIRD_VAR]
  call day6_part1
  add esp, 3*4 
  
  push DWORD 0
  call sys_exit
