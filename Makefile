all:
	gcc main.c -o assembler -Wall -Wextra -std=c99 -g
	./assembler test/simple.pasm test/out.bin
