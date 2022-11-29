all: day1

.PHONY: clean
clean:
	rm *.o day1

day1: day1.o
	ld -static -o day1 day1.o -e _main

day1.o: day1.asm string_utils.asm syscalls.asm integer_utils.asm
	nasm -f macho64 -g -F dwarf	 day1.asm
