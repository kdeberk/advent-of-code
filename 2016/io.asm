%ifndef __IO_ASM__
%define __IO_ASM__

section .bss
  input_buffer resb 65536
  input_bufsize equ $ - input_buffer
  output_buffer resb 1024
  output_bufsize equ $ - output_buffer

section .text

;; println(string, length)
;; Prints the string to stdout, followed by a newline.
println:
;;  copies string to internal buffer, append 0xa and then writes

%endif
