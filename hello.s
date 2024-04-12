	.global _start
	.section .text.bios

_start:	li a1, 0x10000000
	addi a0, x0, 'H'
	sb a0, (a1)

	addi a0, x0, 'e'
	sb a0, (a1)

	addi a0, x0, 'l'
	sb a0, (a1)
	sb a0, (a1)

	addi a0, x0, 'o'
	sb a0, (a1)

	addi a0, x0, ','
	sb a0, (a1)

	addi a0, x0, ' '
	sb a0, (a1)

	addi a0, x0, 'W'
	sb a0, (a1)

	addi a0, x0, 'o'
	sb a0, (a1)

	addi a0, x0, 'r'
	sb a0, (a1)

	addi a0, x0, 'l'
	sb a0, (a1)

	addi a0, x0, 'd'
	sb a0, (a1)

	addi a0, x0, '!'
	sb a0, (a1)

	addi a0, x0, '\n'
	sb a0, (a1)

loop:	jal loop
