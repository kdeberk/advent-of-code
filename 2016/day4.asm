%include "constants.asm"
%include "syscalls.asm"
%include "integer_utils.asm"
%include "string_utils.asm"
%include "shiftcipher.asm"

section .bss
  input_buffer resb 65536
  input_bufsize equ $ - input_buffer
  output_buffer resb 1024
  output_bufsize equ $ - output_buffer
  counts resb 26
  counts_len equ $ - counts
  checksum resb 5
  checksum_len equ $ - checksum
  decrypted resb 1024
  decrypted_len equ $ - decrypted

section .data
  input_file db 'data/day4.txt',0x0
  northpole db 'northpole',0x0
  northpole_len equ $ - northpole - 1

section .text
  global _start


;; args: none
;; returns: nothing
reset_counts:
  cld

  push ebp
  mov ebp, esp

  push eax
  push ecx
  push edi

  xor eax, eax
  mov ecx, counts_len
  mov edi, counts
.loop:
  stosb
  loop .loop

  pop edi
  pop ecx
  pop eax

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - esi: room string
;; returns:
;; - esi: at [ char
;; - counts: filled in counts of times char appeared
count_letters:
  cld

  push ebp
  mov ebp, esp

  push eax

.loop:
  xor eax, eax
  lodsb

  cmp al, '['
  je .return
  cmp al, 'a'
  jl .next

  sub al, 'a'
  add eax, counts
  inc BYTE [eax]                ; counts[al - 'a']++
.next:
  jmp .loop

.return:
  pop eax

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - counts: filled in table
;; vars:
;; - current largest count
;; - index of current largest
;; returns:
;; - checksum: 5 most appearing chars
construct_checksum:
  cld

  push ebp
  mov ebp, esp

  sub esp, 2*4

  push eax
  push ecx
  push esi
  push edi

  mov edi, checksum
.outer_loop:
  mov eax, edi
  sub eax, checksum
  cmp eax, checksum_len
  je .return

  mov DWORD [FIRST_VAR], 0
  mov DWORD [SECOND_VAR], 0x0

  mov esi, counts
  mov ecx, counts_len
.inner_loop:
  xor eax, eax
  mov al, [esi]
  cmp al, [FIRST_VAR]
  jng .inner_next

  mov [FIRST_VAR], eax
  mov [SECOND_VAR], esi
.inner_next:
  inc esi
  loop .inner_loop

  mov eax, [SECOND_VAR]
  sub eax, counts
  add eax, 'a'
  stosb
  mov eax, [SECOND_VAR]
  mov BYTE [eax], 0

  jmp .outer_loop

.return:
  pop edi
  pop esi
  pop ecx
  pop eax

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - start of room string
;; returns:
;; - eax: TRUE if valid, FALSE if it isn't
is_valid_room:
  push ebp
  mov ebp, esp

  push esi

  mov esi, [SINGLE_ARG]

  call reset_counts
  call count_letters
  call construct_checksum

  push checksum
  push esi
  push checksum_len
  call strcmp_len
  add esp, 3*4

  pop esi

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - char array with rooms
;; - n of rooms
;; vars:
;; - count of valid rooms
;; returns:
;; - n of valid rooms
remove_invalid_rooms:
  push ebp
  mov ebp, esp

  sub esp, 1*4

  push ecx
  push esi
  push edi

  mov ecx, [SECOND_OF_TWO_ARGS]
  mov DWORD [FIRST_VAR], 0
  mov esi, [FIRST_OF_TWO_ARGS]
  mov edi, [FIRST_OF_TWO_ARGS]

.loop:
  push esi
  call is_valid_room
  add esp, 1*4

  cmp eax, TRUE
  jne .read_until_next_room

  push DWORD 0xa
  call copy_string_until
  add esp, 1*4

  inc DWORD [FIRST_VAR]
  jmp .next
.read_until_next_room:
  push DWORD 0xa
  call skip_chars_until
  add esp, 1*4
.next:
  loop .loop

  mov eax, 0x0
  stosb
  mov eax, [FIRST_VAR]

  pop edi
  pop esi
  pop ecx

  mov esp, ebp
  pop ebp
  ret


;; args:
;; - valid rooms, separated by 0xa
;; - n valid rooms
;; vars:
;; - sum of valid room codes
;; returns:
;; - eax: sum of valid room codes
day4_part1:
  push ebp
  mov ebp, esp

  sub esp, 1*4

  mov esi, [FIRST_OF_TWO_ARGS]
  mov ecx, [SECOND_OF_TWO_ARGS]
  cmp ecx, 0
  je .end

  mov DWORD [FIRST_VAR], 0
.loop:
  call skip_chars_until_digits
  call read_integer

  add eax, [FIRST_VAR]
  mov [FIRST_VAR], eax

  loop .loop
.end:
  mov esp, ebp
  pop ebp
  ret


;; args:
;; - valid rooms, separated by 0xa
;; - n valid rooms
;; vars:
;; - current index of [
;; - current room code
;; returns:
;; - room number that contains the magic word
day4_part2:
  push ebp
  mov ebp, esp

  sub esp, 2*4

  push ecx
  push esi

  mov esi, [FIRST_OF_TWO_ARGS]
  mov ecx, [SECOND_OF_TWO_ARGS]
.loop:
  push esi
  push '['
  call str_index_of_char
  add esp, 2*4
  mov [FIRST_VAR], eax

  push esi
  call skip_chars_until_digits
  call read_integer
  mov [SECOND_VAR], eax
  pop esi

  push esi
  push DWORD [FIRST_VAR]
  push decrypted
  push DWORD [SECOND_VAR]
  call shift_string
  add esp, 4*4

  push decrypted
  push northpole
  push northpole_len
  call strcmp_len
  add esp, 3*4
  cmp eax, TRUE
  je .found

  push 0xa
  call skip_chars_until
  add esp, 1*4
  loop .loop
  jmp .notfound

.found:
  mov esi, decrypted
  call skip_chars_until_digits
  call read_integer
  jmp .return

.notfound:
  mov eax, -1

.return:
  pop esi
  pop ecx

  mov esp, ebp
  pop ebp
  ret


;; args: none
;; vars:
;; - number of rooms
;; returns: none
_start:
  push ebp
  mov ebp, esp

  push input_file               ; read input file
  push input_buffer
  push input_bufsize
  call read_file
  add esp, 3*4

  push input_buffer             ; count lines
  push DWORD 0xa
  call count_specific_char
  mov DWORD [FIRST_VAR], eax

  push input_buffer
  push eax
  call remove_invalid_rooms
  add esp, 2*4
  mov DWORD [FIRST_VAR], eax

  push input_buffer
  push DWORD [FIRST_VAR]
  call day4_part1
  add esp, 2*4

  push eax
  call print_integer
  add esp, 1*4

  push input_buffer
  push DWORD [FIRST_VAR]
  call day4_part2
  add esp, 2*4

  push eax
  call print_integer
  add esp, 1*4


  push DWORD 0
  call sys_exit
