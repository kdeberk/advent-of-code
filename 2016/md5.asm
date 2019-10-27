%ifndef __MD5__
%define __MD5__
%include "constants.asm"

;; N.B. only supporting messages with max length of 55 bytes

N_BITS_PER_BYTE equ 8
N_BYTES_PER_DWORD equ 4
N_BYTES_PER_QWORD equ 8

%define A (ebp-4)
%define B (ebp-8)
%define C (ebp-12)
%define D (ebp-16)
%define AA (ebp-20)
%define BB (ebp-24)
%define CC (ebp-28)
%define _DD (ebp-32)

%macro F 5                      ; [AB k s i], F is in eax
  add eax, [%1]                 ; a = b + ((a + F + X[k] + T[i])) <<< s
  add eax, [m+4*%3]
  add eax, [T+4*%5]

  mov cl, %4
  rol eax, cl

  add eax, [%2]
  mov [%1], eax
%endmacro

%macro round_1 7                ; [ ABCD k s i]
  mov eax, [%2]                 ; F = (B∧C)∨(¬B∧D)
  and eax, [%3]
  mov ebx, [%2]
  not ebx
  and ebx, [%4]
  or eax, ebx 
  F %1, %2, %5, %6, %7
%endmacro

%macro round_2 7                ; [ ABCD k s i]
  mov eax, [%4]                 ; F = (D∧B)∨(¬D∧C)
  and eax, [%2]
  mov ebx, [%4]
  not ebx
  and ebx, [%3]
  or eax, ebx
  F %1, %2, %5, %6, %7  
%endmacro

%macro round_3 7                ; [ ABCD k s i]
  mov eax, [%2]                 ; F = B⊕C⊕D 
  xor eax, [%3]
  xor eax, [%4]
  F %1, %2, %5, %6, %7  
%endmacro

%macro round_4 7                ; [ ABCD k s i]
  mov eax, [%4]                 ; F = C⊕(B∨(¬D))
  not eax
  or eax, [%2]
  xor eax, [%3]
  F %1, %2, %5, %6, %7  
%endmacro


section .bss
  m resb 64                     ; 512 bits
  m_len equ $ - m

section .data
  initial_A dd 0x67452301
  initial_B dd 0xEFCDAB89
  initial_C dd 0x98BADCFE
  initial_D dd 0x10325476

  ; T in formula is 1-based
  T dd 0x00000000, \
       0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, \
       0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501, \
       0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, \
       0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, \
       0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, \
       0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8, \
       0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, \
       0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a, \
       0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, \
       0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, \
       0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, \
       0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665, \
       0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, \
       0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1, \
       0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, \
       0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391

section .text
;; args:
;; - plaintext: chars to encode
;; - plaintext_len: size of buffer
;; - destination: 16 byte array to write result to
md5_hash:
  push ebp
  mov ebp, esp

  sub esp, 8*4

  push ebx
  push ecx
  push edx
  push esi
  push edi

  ; Prepare A, B, C and D
  mov eax, [initial_A]
  mov [A], eax
  mov eax, [initial_B]
  mov [B], eax
  mov eax, [initial_C]
  mov [C], eax
  mov eax, [initial_D]
  mov [D], eax

  push DWORD [FIRST_OF_THREE_ARGS]
  push DWORD [SECOND_OF_THREE_ARGS]
  call prepare_message
  add esp, 2*4

.block_loop:
  mov eax, [A]
  mov [AA], eax
  mov eax, [B]
  mov [BB], eax
  mov eax, [C]
  mov [CC], eax
  mov eax, [D]
  mov [_DD], eax

  round_1 A, B, C, D,  0,  7,  1
  round_1 D, A, B, C,  1, 12,  2
  round_1 C, D, A, B,  2, 17,  3
  round_1 B, C, D, A,  3, 22,  4

  round_1 A, B, C, D,  4,  7,  5
  round_1 D, A, B, C,  5, 12,  6
  round_1 C, D, A, B,  6, 17,  7
  round_1 B, C, D, A,  7, 22,  8

  round_1 A, B, C, D,  8,  7,  9
  round_1 D, A, B, C,  9, 12, 10
  round_1 C, D, A, B, 10, 17, 11
  round_1 B, C, D, A, 11, 22, 12

  round_1 A, B, C, D, 12,  7, 13
  round_1 D, A, B, C, 13, 12, 14
  round_1 C, D, A, B, 14, 17, 15
  round_1 B, C, D, A, 15, 22, 16

  round_2 A, B, C, D,  1,  5, 17
  round_2 D, A, B, C,  6,  9, 18
  round_2 C, D, A, B, 11, 14, 19
  round_2 B, C, D, A,  0, 20, 20

  round_2 A, B, C, D,  5,  5, 21
  round_2 D, A, B, C, 10,  9, 22
  round_2 C, D, A, B, 15, 14, 23
  round_2 B, C, D, A,  4, 20, 24

  round_2 A, B, C, D,  9,  5, 25
  round_2 D, A, B, C, 14,  9, 26
  round_2 C, D, A, B,  3, 14, 27
  round_2 B, C, D, A,  8, 20, 28

  round_2 A, B, C, D, 13,  5, 29
  round_2 D, A, B, C,  2,  9, 30
  round_2 C, D, A, B,  7, 14, 31
  round_2 B, C, D, A, 12, 20, 32

  round_3 A, B, C, D,  5,  4, 33
  round_3 D, A, B, C,  8, 11, 34
  round_3 C, D, A, B, 11, 16, 35
  round_3 B, C, D, A, 14, 23, 36

  round_3 A, B, C, D,  1,  4, 37
  round_3 D, A, B, C,  4, 11, 38
  round_3 C, D, A, B,  7, 16, 39
  round_3 B, C, D, A, 10, 23, 40

  round_3 A, B, C, D, 13,  4, 41
  round_3 D, A, B, C,  0, 11, 42
  round_3 C, D, A, B,  3, 16, 43
  round_3 B, C, D, A,  6, 23, 44

  round_3 A, B, C, D,  9,  4, 45
  round_3 D, A, B, C, 12, 11, 46
  round_3 C, D, A, B, 15, 16, 47
  round_3 B, C, D, A,  2, 23, 48

  round_4 A, B, C, D,  0,  6, 49
  round_4 D, A, B, C,  7, 10, 50
  round_4 C, D, A, B, 14, 15, 51
  round_4 B, C, D, A,  5, 21, 52

  round_4 A, B, C, D, 12,  6, 53
  round_4 D, A, B, C,  3, 10, 54
  round_4 C, D, A, B, 10, 15, 55
  round_4 B, C, D, A,  1, 21, 56

  round_4 A, B, C, D,  8,  6, 57
  round_4 D, A, B, C, 15, 10, 58
  round_4 C, D, A, B,  6, 15, 59
  round_4 B, C, D, A, 13, 21, 60

  round_4 A, B, C, D,  4,  6, 61
  round_4 D, A, B, C, 11, 10, 62
  round_4 C, D, A, B,  2, 15, 63
  round_4 B, C, D, A,  9, 21, 64

  mov eax, [A]
  add eax, [AA]
  mov [A], eax
  mov eax, [B]
  add eax, [BB]
  mov [B], eax
  mov eax, [C]
  add eax, [CC]
  mov [C], eax
  mov eax, [D]
  add eax, [_DD]
  mov [D], eax

  mov ebx, [THIRD_OF_THREE_ARGS]
  mov eax, [A]          ; store ABCD in checksum
  mov [ebx], eax
  mov eax, [B]
  mov [ebx+4], eax
  mov eax, [C]
  mov [ebx+8], eax
  mov eax, [D]
  mov [ebx+12], eax

  pop edi
  pop esi
  pop edx
  pop ecx
  pop ebx

  mov esp, ebp
  pop ebp
  ret

;; copy message and add padding
;; args:
;; - plain text
;; - plain text length
prepare_message:
  push ebp
  mov ebp, esp

  mov ecx, [SECOND_OF_TWO_ARGS]
  mov esi, [FIRST_OF_TWO_ARGS]
  mov edi, m
.copy_message_loop:
  lodsb
  stosb
  loop .copy_message_loop

  ; pad 1 bit (+ 7 0 bits)
  mov eax, 0x80
  stosb

  ; calculate distance to start of length
  ; TODO: replace ecx+loop with cmp with desired padding length and jne
  mov ecx, [FIRST_OF_TWO_ARGS]
  add ecx, 448 / N_BITS_PER_BYTE
  sub ecx, esi
  dec ecx

  xor eax, eax
 .zero_padding_loop:
  stosb
  loop .zero_padding_loop

  mov eax, [SECOND_OF_TWO_ARGS]
  imul eax, N_BITS_PER_BYTE
  mov ecx, N_BYTES_PER_QWORD
.length_padding_loop:
  stosb
  shr eax, N_BITS_PER_BYTE
loop .length_padding_loop

  mov esp, ebp
  pop ebp
  ret

%endif
