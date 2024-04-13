out/hello.txt: out/hello.bin
	hexdump out/hello.bin -e '/1 "%00u" "\n"' -v > out/hello.txt

out/hello.bin: out/hello.o
	riscv32-unknown-linux-gnu-objcopy -O binary out/hello.o out/hello.bin

out/hello.o: hello.c memmap.ld
	riscv32-unknown-linux-gnu-gcc -nostdlib -ffreestanding -march=rv32i -mabi=ilp32 -Tmemmap.ld -O1 -o out/hello.o hello.c

hello.c:

memmap.ld:

clean:
	rm -f out/*.o
	rm -f out/*.bin
