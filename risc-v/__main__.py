from boiga import *

project = Project()

emu = project.new_sprite("RISCV32")

DRAM_SIZE = 4 * 1000 * 1000 # 4 MB
DRAM_BASE = 0x80000000

dram = emu.new_list("_DRAM", [0] * DRAM_SIZE)
regs = emu.new_list("_REGS", [0] * 32)
pc = emu.new_var("_PC")

code = emu.new_list("_CODE")

uart = emu.new_list("_UART")

and_lut_contents = []
for a in range(256):
                for b in range(256):
                                and_lut_contents.append(a & b)

and_lut = emu.new_list("_AND_LUT", and_lut_contents)

or_lut_contents = []
for a in range(256):
                for b in range(256):
                                or_lut_contents.append(a | b)

or_lut = emu.new_list("_OR_LUT", or_lut_contents)

xor_lut_contents = []
for a in range(256):
                for b in range(256):
                                xor_lut_contents.append(a ^ b)

xor_lut = emu.new_list("_XOR_LUT", xor_lut_contents)

@emu.proc_def()
def reset (locals): return [
                locals.i[:DRAM_SIZE:1] >> [
                                dram[locals.i] <= 0
                ],
                locals.i[:code.len():1] >> [
                                dram[locals.i+0x0] <= code[locals.i]
                ],
                locals.i[:32:1] >> [
                                regs[locals.i] <= 0
                ],
                regs[2] <= DRAM_SIZE,
                pc <= DRAM_BASE
]

### Functions for sign extension

@emu.proc_def(inline_only=True)
def toSigned32 (locals, int): return [
                If (int > 2147483647) [
                                locals.result <= -4294967296 + int
                ].Else [
                                locals.result <= int
                ]
]

@emu.proc_def(inline_only=True)
def toSigned16 (locals, int): return [
                If (int > 32767) [
                                locals.result <= -65536 + int
                ].Else [
                                locals.result <= int
                ]
]

@emu.proc_def(inline_only=True)
def toSigned8 (locals, int): return [
                If (int > 127) [
                                locals.result <= -256 + int
                ].Else [
                                locals.result <= int
                ]
]

@emu.proc_def(inline_only=True)
def toUnsigned32 (locals, int): return [
                If (int < 0) [
                                locals.result <= 4294967296 + int
                ].Else [
                                locals.result <= int
                ]
]

### ALU operations

@emu.proc_def(inline_only=True)
def add (locals, a, b): return [
                locals.result <= (a + b) & 0xffffffff
]

@emu.proc_def(inline_only=True)
def sub (locals, a, b): return [
                locals.result <= (a - b) & 0xffffffff,
                If (locals.result < 0) [
                                locals.result <= 0
                ]
]

@emu.proc_def(inline_only=True)
def b_xor (locals, a, b): return [
                locals.result <= xor_lut[(a & 0xff) + ((b & 0xff) * 256)] # 0:7
                                + (xor_lut[((a >> 8) & 0xff) + (((b >> 8) & 0xff) * 256)] << 8) # 8:15
                                + (xor_lut[((a >> 16) & 0xff) + (((b >> 16) & 0xff) * 256)] << 16) # 16:23
                                + (xor_lut[((a >> 24) & 0xff) + (((b >> 24) & 0xff) * 256)] << 24) # 24:31
]

@emu.proc_def(inline_only=True)
def b_or (locals, a, b): return [
                locals.result <= or_lut[(a & 0xff) + ((b & 0xff) * 256)] # 0:7
                                + (or_lut[((a >> 8) & 0xff) + (((b >> 8) & 0xff) * 256)] << 8) # 8:15
                                + (or_lut[((a >> 16) & 0xff) + (((b >> 16) & 0xff) * 256)] << 16) # 16:23
                                + (or_lut[((a >> 24) & 0xff) + (((b >> 24) & 0xff) * 256)] << 24) # 24:31
]

@emu.proc_def(inline_only=True)
def b_and (locals, a, b): return [
                locals.result <= and_lut[(a & 0xff) + ((b & 0xff) * 256)] # 0:7
                                + (and_lut[((a >> 8) & 0xff) + (((b >> 8) & 0xff) * 256)] << 8) # 8:15
                                + (and_lut[((a >> 16) & 0xff) + (((b >> 16) & 0xff) * 256)] << 16) # 16:23
                                + (and_lut[((a >> 24) & 0xff) + (((b >> 24) & 0xff) * 256)] << 24) # 24:31
]

@emu.proc_def(inline_only=True)
def b_shift_left (locals, a, b): return [
                locals.result <= (a << b) & 0xffffffff
]

@emu.proc_def(inline_only=True)
def b_shift_right (locals, a, b): return [
                locals.result <= a >> b
]

@emu.proc_def(inline_only=True)
def b_shift_right_arith (locals, a, b): return [
                toSigned32(a).inline(),
                locals.result <= toSigned32.result >> b
]

@emu.proc_def(inline_only=True)
def less_than_unsigned (locals, a, b): return [
                If (a < b) [
                                locals.result <= 1
                ].Else [
                                locals.result <= 0
                ]
]

@emu.proc_def(inline_only=True)
def less_than_signed (locals, a, b): return [
                toSigned32(a).inline(),
                locals.signed <= toSigned32.result,
                toSigned32(b).inline(),
                If (locals.signed < toSigned32.result) [
                                locals.result <= 1
                ].Else [
                                locals.result <= 0
                ]
]

### Memory and bus operations

bus_result = emu.new_var("_bus_result")

@emu.proc_def(inline_only=True)
def mem_load32 (locals, index): return [
                bus_result <= dram[index]
                + (dram[index+1] << 8)
                + (dram[index+2] << 16)
                + (dram[index+3] << 24)
]

@emu.proc_def(inline_only=True)
def mem_load16 (locals, index): return [
                bus_result <= dram[index]
                + (dram[index+1] << 8)
]

@emu.proc_def(inline_only=True)
def mem_load8 (locals, index): return [
                bus_result <= dram[index]
]

@emu.proc_def()
def hw_load8 (locals, addr): return []

@emu.proc_def(inline_only=True)
def bus_load32 (locals, addr): return [
                If (addr < DRAM_BASE) [
                                hw_load8(addr),
                                locals.result <= bus_result,
                                hw_load8(addr),
                                locals.result <= locals.result + (bus_result << 8),
                                hw_load8(addr),
                                locals.result <= locals.result + (bus_result << 16),
                                hw_load8(addr),
                                bus_result <= locals.result + (bus_result << 24)
                ].Else [
                                mem_load32(addr - DRAM_BASE).inline()
                ]
]

@emu.proc_def(inline_only=True)
def bus_load16 (locals, addr): return [
                If (addr < DRAM_BASE) [
                                hw_load8(addr),
                                locals.result <= bus_result,
                                hw_load8(addr),
                                bus_result <= locals.result + (bus_result << 8)
                ].Else [
                                mem_load16(addr - DRAM_BASE).inline()
                ]
]

@emu.proc_def(inline_only=True)
def bus_load8 (locals, addr): return [
                If (addr < DRAM_BASE) [
                                hw_load8(addr)
                ].Else [
                                mem_load8(addr - DRAM_BASE).inline()
                ]
]

@emu.proc_def(inline_only=True)
def mem_store8 (locals, index, value): return [
                dram[index] <= value
]

@emu.proc_def()
def hw_store8 (locals, addr, value): return [
                If (addr == 0x10000000) [
                                uart.append(value)
                ]
]

@emu.proc_def(inline_only=True)
def bus_store32 (locals, addr, value): return [
                If (addr < DRAM_BASE) [
                                hw_store8(addr, value & 0xff),
                                hw_store8(addr + 1, (value >> 8) & 0xff),
                                hw_store8(addr + 2, (value >> 16) & 0xff),
                                hw_store8(addr + 3, (value >> 24) & 0xff)
                ].Else [
                                mem_store8(addr - DRAM_BASE, value & 0xff).inline(),
                                mem_store8(addr - DRAM_BASE + 1, (value >> 8) & 0xff).inline(),
                                mem_store8(addr - DRAM_BASE + 2, (value >> 16) & 0xff).inline(),
                                mem_store8(addr - DRAM_BASE + 3, (value >> 24) & 0xff).inline()
                ]
]

@emu.proc_def(inline_only=True)
def bus_store16 (locals, addr, value): return [
                If (addr < DRAM_BASE) [
                                hw_store8(addr, value & 0xff),
                                hw_store8(addr, (value >> 8) & 0xff)
                ].Else [
                                mem_store8(addr - DRAM_BASE, value & 0xff).inline(),
                                mem_store8(addr - DRAM_BASE + 1, (value >> 8) & 0xff).inline()
                ]
]

@emu.proc_def(inline_only=True)
def bus_store8 (locals, addr, value): return [
                If (addr < DRAM_BASE) [
                                hw_store8(addr, value & 0xff)
                ].Else [
                                mem_store8(addr - DRAM_BASE, value & 0xff).inline()
                ]
]

@emu.proc_def(inline_only=True)
def fetch (locals, pc): return [
                mem_load32(pc - DRAM_BASE).inline()
]

### Decoders

@emu.proc_def(inline_only=True)
def decode_r_type (locals, inst): return [
                execute.rd <= (inst >> 7) & 0x1f,
                execute.rs1 <= (inst >> 15) & 0x1f,
                execute.rs2 <= (inst >> 20) & 0x1f,
                execute.funct3 <= (inst >> 12) & 0x7,
                execute.funct7 <= inst >> 25
]


@emu.proc_def(inline_only=True)
def decode_i_type (locals, inst): return [
                execute.rd <= (inst >> 7) & 0x1f,
                execute.rs1 <= (inst >> 15) & 0x1f,
                execute.funct3 <= (inst >> 12) & 0x7,
                toSigned32(inst).inline(),
                toUnsigned32(toSigned32.result >> 20).inline(),
                execute.imm <= toUnsigned32.result
]

@emu.proc_def(inline_only=True)
def decode_s_type (locals, inst): return [
                execute.rs1 <= (inst >> 15) & 0x1f,
                execute.rs2 <= (inst >> 20) & 0x1f,
                execute.funct3 <= (inst >> 12) & 0x7,
                toSigned32(inst).inline(),
                toUnsigned32(toSigned32.result >> 25).inline(),
                execute.imm <= (toUnsigned32.result << 5) + ((inst >> 7) & 0x1f)
]

@emu.proc_def(inline_only=True)
def decode_b_type (locals, inst): return [
                execute.rs1 <= (inst >> 15) & 0x1f,
                execute.rs2 <= (inst >> 20) & 0x1f,
                execute.funct3 <= (inst >> 12) & 0x7,
                toSigned32(inst).inline(),
                toUnsigned32(toSigned32.result >> 31).inline(),
                execute.imm <= (toUnsigned32.result << 12) + (((inst >> 7) & 0x01) << 11) + (((inst >> 25) & 0x1f) << 5) + (((inst >> 8) & 0x0f) << 1)
]

@emu.proc_def(inline_only=True)
def decode_u_type (locals, inst): return [
                execute.rd <= (inst >> 7) & 0x1f,
                execute.imm <= (inst >> 12) << 12
]

@emu.proc_def(inline_only=True)
def decode_j_type (locals, inst): return [
                execute.rd <= (inst >> 7) & 0x1f,
                toSigned32(inst).inline(),
                toUnsigned32(toSigned32.result >> 31).inline(),
                execute.imm <= (toUnsigned32.result << 20) + (((inst >> 12) & 0xff) << 12) + (((inst >> 20) & 0x01) << 11) + ((inst >> 21) & 0x03ff) << 1
]

@emu.proc_def(inline_only=True)
def execute (locals, inst): return [
                locals.opcode <= inst & 0x7f,
                If (locals.opcode == 0b0110011) [
                                decode_r_type(inst).inline(),
                                If (locals.funct3 == 0x0) [
                                                If (locals.funct7 == 0x0) [ # add
                                                                add(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                                regs[locals.rd] <= add.result
                                                ],
                                                If (locals.funct7 == 0x20) [ # sub
                                                                sub(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                                regs[locals.rd] <= sub.result
                                                ]
                                ],
                                If (locals.funct3 == 0x4) [ # xor
                                                b_xor(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= b_xor.result
                                ],
                                If (locals.funct3 == 0x6) [ # or
                                                b_or(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= b_or.result
                                ],
                                If (locals.funct3 == 0x7) [ # and
                                                b_and(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= b_and.result
                                ],
                                If (locals.funct3 == 0x1) [ # sll
                                                b_shift_left(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= b_shift_left.result
                                ],
                                If (locals.funct3 == 0x5) [
                                                If (locals.funct7 == 0x0) [ # srl
                                                                b_shift_right(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                                regs[locals.rd] <= b_shift_right.result
                                                ],
                                                If (locals.funct7 == 0x20) [ # sra
                                                                b_shift_right_arith(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                                regs[locals.rd] <= b_shift_right_arith.result
                                                ]
                                ],
                                If (locals.funct3 == 0x2) [ # slt
                                                less_than_signed(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= less_than_signed.result
                                ],
                                If (locals.funct3 == 0x3) [ # sltu
                                                less_than_unsigned(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= less_than_unsigned.result
                                ]
                ],
                If (locals.opcode == 0b0010011) [
                                decode_i_type(inst).inline(),
                                If (locals.funct3 == 0x0) [ # addi
                                                add(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= add.result
                                ],
                                If (locals.funct3 == 0x4) [ # xori
                                                b_xor(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= b_xor.result
                                ],
                                If (locals.funct3 == 0x6) [ # ori
                                                b_or(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= b_or.result
                                ],
                                If (locals.funct3 == 0x7) [ # andi
                                                b_and(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= b_and.result
                                ],
                                If (locals.funct3 == 0x1) [ # slli
                                                b_shift_left(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= b_shift_left.result
                                ],
                                If (locals.funct3 == 0x5) [
                                                locals.funct7 <= inst >> 25,
                                                If (locals.funct7 == 0x0) [ # srli
                                                                b_shift_right(regs[locals.rs1], locals.imm).inline(),
                                                                regs[locals.rd] <= b_shift_right.result
                                                ],
                                                If (locals.funct7 == 0x20) [ # srai
                                                                b_shift_right_arith(regs[locals.rs1], locals.imm & 0x1f).inline(),
                                                                regs[locals.rd] <= b_shift_right_arith.result
                                                ]
                                ],
                                If (locals.funct3 == 0x2) [ # slti
                                                less_than_signed(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= less_than_signed.result
                                ],
                                If (locals.funct3 == 0x3) [ # sltiu
                                                less_than_unsigned(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= less_than_unsigned.result
                                ]
                ],
                If (locals.opcode == 0b0000011) [
                                decode_i_type(inst).inline(),
                                add(regs[locals.rs1], locals.imm).inline(),
                                locals.addr <= add.result,
                                If (locals.funct3 == 0x0) [ # lb
                                                bus_load8(locals.addr).inline(),
                                                toSigned8(bus_result).inline(),
                                                toUnsigned32(toSigned8.result).inline(),
                                                regs[locals.rd] <= toUnsigned32.result
                                ],
                                If (locals.funct3 == 0x1) [ # lh
                                                bus_load16(locals.addr).inline(),
                                                toSigned16(bus_result).inline(),
                                                toUnsigned32(toSigned16.result).inline(),
                                                regs[locals.rd] <= toUnsigned32.result
                                ],
                                If (locals.funct3 == 0x2) [ # lw
                                                bus_load32(locals.addr).inline(),
                                                regs[locals.rd] <= bus_result
                                ],
                                If (locals.funct3 == 0x4) [ # lbu
                                                bus_load8(locals.addr).inline(),
                                                regs[locals.rd] <= bus_result
                                ],
                                If (locals.funct3 == 0x5) [ # lhu
                                                bus_load16(locals.addr).inline(),
                                                regs[locals.rd] <= bus_result
                                ]
                ],
                If (locals.opcode == 0b0100011) [
                                decode_s_type(inst).inline(),
                                add(regs[locals.rs1], locals.imm).inline(),
                                locals.addr <= add.result,
                                If (locals.funct3 == 0x0) [ # sb
                                                bus_store8(locals.addr, regs[locals.rs2] & 0xff).inline()
                                ],
                                If (locals.funct3 == 0x1) [ # sh
                                                bus_store16(locals.addr, regs[locals.rs2] & 0xffff).inline()
                                ],
                                If (locals.funct3 == 0x2) [ # sw
                                                bus_store32(locals.addr, regs[locals.rs2]).inline()
                                ]
                ],
                If (locals.opcode == 0b1100011) [
                                decode_b_type(inst).inline(),
                                If (locals.funct3 == 0x0) [ # beq
                                                If (regs[locals.rs1] == regs[locals.rs2]) [
                                                                pc <= pc + locals.imm
                                                ]
                                ],
                                If (locals.funct3 == 0x1) [ # bne
                                                If (regs[locals.rs1] != regs[locals.rs2]) [
                                                                pc <= pc + locals.imm
                                                ]
                                ],
                                If (locals.funct3 == 0x6) [ # bltu
                                                If (regs[locals.rs1] < regs[locals.rs2]) [
                                                                pc <= pc + locals.imm
                                                ]
                                ],
                                If (locals.funct3 == 0x7) [ # bgeu
                                                If ((regs[locals.rs1] < regs[locals.rs2]).NOT()) [
                                                                pc <= pc + locals.imm
                                                ]
                                ],
                                If (locals.funct3 == 0x4) [ # blt
                                                toSigned32(regs[locals.rs1]).inline(),
                                                locals.srs1 <= toSigned32.result,
                                                toSigned32(regs[locals.rs2]).inline(),
                                                If (locals.srs1 < toSigned32.result) [
                                                                pc <= pc + locals.imm
                                                ]
                                ],
                                If (locals.funct3 == 0x5) [ # bge
                                                toSigned32(regs[locals.rs1]).inline(),
                                                locals.srs1 <= toSigned32.result,
                                                toSigned32(regs[locals.rs2]).inline(),
                                                If ((locals.srs1 < toSigned32.result).NOT()) [
                                                                pc <= pc + locals.imm
                                                ]
                                ]
                ],
                If (locals.opcode == 0b1101111) [ # jal
                                decode_j_type(inst).inline(),
                                regs[locals.rd] <= pc + 4,
                                pc <= pc + locals.imm
                ],
                If (locals.opcode == 0b1101111) [ # jalr
                                decode_i_type(inst).inline(),
                                regs[locals.rd] <= pc + 4,
                                pc <= regs[locals.rs1] + locals.imm
                ],
                If (locals.opcode == 0b0110111) [ # lui
                                decode_u_type(inst).inline(),
                                regs[locals.rd] <= locals.imm
                ],
                If (locals.opcode == 0b0010111) [ # auipc
                                decode_u_type(inst).inline(),
                                regs[locals.rd] <= locals.imm + pc
                ]
]

@emu.proc_def()
def tick (locals): return [
                If (pc - DRAM_BASE > DRAM_SIZE) [
                             StopAll()   
                ],
                regs[0] <= 0,
                fetch(pc).inline(),
                pc <= pc + 4,
                execute(bus_result).inline()
]

@emu.proc_def()
def loop (locals): return [
                Repeat (1000) [
                tick()
                ]
]

emu.on_flag([
            reset(),
            Forever [
            loop()
            ]
])

project.save("out/risc-v.sb3")
