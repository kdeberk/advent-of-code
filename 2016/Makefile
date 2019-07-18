.PHONY: clean run

all: run

run: day1 day2
	./day1
	./day2

day1: day1.o
	ld -m elf_i386 -o day1 day1.o

day2: day2.o
	ld -m elf_i386 -o day2 day2.o

day1.o: day1.asm constants.asm syscalls.asm integer_utils.asm string_utils.asm
	nasm -f elf -F dwarf -g day1.asm

day2.o: day2.asm constants.asm syscalls.asm
	nasm -f elf -F dwarf -g day2.asm

clean:
	rm *.o day1 day2
