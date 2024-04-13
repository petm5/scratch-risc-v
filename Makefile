out/hello.bin: out/hello.o
	riscv32-unknown-linux-gnu-objcopy -O binary -j .text.bios out/hello.o out/hello.bin

out/hello.o:
	riscv32-unknown-linux-gnu-gcc -Wl,-Ttext=0x0 -nostdlib -march=rv32i -mabi=ilp32 -o out/hello.o -c hello.s

clean:
	rm -f out/*.o
	rm -f out/*.bin
