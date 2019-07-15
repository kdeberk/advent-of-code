.PHONY: clean run

all: run

run: day1
	./day1

day1: day1.o
	ld -m elf_i386 -o day1 day1.o

day1.o: day1.asm
	nasm -f elf -F dwarf -g day1.asm

clean:
	rm *.o day1
