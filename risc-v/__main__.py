from boiga import *

project = Project()

emu = project.new_sprite("RISCV32")

DRAM_SIZE = 800000
DRAM_BASE = 0x80000000

dram = emu.new_list("_DRAM", [0] * DRAM_SIZE)
regs = emu.new_list("_REGS", [0] * 32)
csrs = emu.new_list("_CSRS", [0] * 4096)
pc = emu.new_var("_PC")
ticks = emu.new_var("_TICKS")

code = emu.new_list("_CODE", monitor=[240, 145, 120, 20])

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

ascii_lut = emu.new_var("_ASCII", ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz')
hex_lut = emu.new_var("_HEXA", '0123456789abcdef')

@emu.proc_def()
def reset (locals): return [
                uart.delete_all(),
                uart.append(""),
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
                pc <= DRAM_BASE,
                ticks <= 0
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
                locals.result <= ((int + 2147483648) % 4294967296) - 2147483648
]

### ALU operations

@emu.proc_def(inline_only=True)
def add (locals, a, b): return [
                locals.result <= (a + b) & 0xffffffff
]

@emu.proc_def(inline_only=True)
def sub (locals, a, b): return [
                toUnsigned32((a - b) & 0xffffffff).inline(),
                locals.result <= toUnsigned32.result
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
                toUnsigned32(toSigned32.result >> b).inline(),
                locals.result <= toUnsigned32.result
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
                If (addr == 0x10002000) [
                                locals.x <= value
                ],
                If (addr == 0x10002001) [
                                SetXYPos(locals.x * 1.88 - 240, value * 1.4 - 180)
                ],
                If (addr == 0x10002002) [
                                SetPenParam("color", value),
                                PenDown()
                ],
                If (addr == 0x10002003) [
                                If (value == 0) [
                                                PenUp()
                                ].Else [
                                                SetPenSize(value)
                                ]
                ],
                If (addr == 0x10002004) [
                                EraseAll()
                ],
                If (addr == 0x10000000) [
                                If (value == 10) [
                                                uart.append("")
                                ].Else [
                                                uart[uart.len()-1] <= uart[uart.len()-1].join(ascii_lut[value-32])
                                ]
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
                execute.funct3 <= (inst >> 12) & 0x7
]

@emu.proc_def(inline_only=True)
def decode_i_type_signed (locals, inst): return [
                toSigned32(inst).inline(),
                execute.imm <= toSigned32.result >> 20
]

@emu.proc_def(inline_only=True)
def decode_i_type_unsigned (locals, inst): return [
                toSigned32(inst).inline(),
                toUnsigned32(toSigned32.result >> 20).inline(),
                execute.imm <= toUnsigned32.result
]

@emu.proc_def(inline_only=True)
def decode_i_type_csr (locals, inst): return [
                execute.rd <= (inst >> 7) & 0x1f,
                execute.rs1 <= (inst >> 15) & 0x1f,
                execute.funct3 <= (inst >> 12) & 0x7,
                execute.imm <= inst >> 20
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
                execute.imm <= ((toSigned32.result >> 31) << 12) + (((inst >> 7) & 0x01) << 11) + (((inst >> 25) & 0x3f) << 5) + (((inst >> 8) & 0x0f) << 1)
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
                execute.imm <= ((toSigned32.result >> 31) << 20) + (((inst >> 12) & 0xff) << 12) + (((inst >> 20) & 0x01) << 11) + (((inst >> 21) & 0x03ff) << 1)
]

# cnt = emu.new_list("instruction count")
# freq = emu.new_list("instruction frequency")

# @emu.proc_def()
# def freq (locals): return [
#                 freq.delete_all(),
#                 RepeatUntil (cnt.len() == 0) [
#                                 locals.largest <= 0,
#                                 locals.largest_i <= 0,
#                                 locals.i[:cnt.len()/2:2] >> [
#                                                If (cnt[locals.i+1] > locals.largest) [
#                                                                locals.largest <= cnt[locals.i+1],
#                                                                locals.largest_i <= locals.i
#                                                ]
#                                 ],
#                                 freq.append(cnt[locals.largest_i]),
#                                 cnt.delete_at(locals.largest_i),
#                                 cnt.delete_at(locals.largest_i)
#                 ]
# ]

@emu.proc_def(inline_only=True)
def execute (locals, inst): return [
                locals.opcode <= inst & 0x7f,
                locals.matched <= 0,
                # locals.i <= cnt.index(Literal("o").join(locals.opcode)),
                # If (locals.i == -1) [
                #                 cnt.append(Literal("o").join(locals.opcode)),
                #                 cnt.append(1)
                # ].Else [
                #                 cnt[locals.i+1] <= cnt[locals.i+1] + 1
                # ],
                If (locals.opcode == 0b0010011) [
                                decode_i_type(inst).inline(),
                                If (locals.funct3 == 0x0) [ # addi
                                                decode_i_type_signed(inst).inline(),
                                                add(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= add.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x4) [ # xori
                                                decode_i_type_unsigned(inst).inline(),
                                                b_xor(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= b_xor.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x6) [ # ori
                                                decode_i_type_unsigned(inst).inline(),
                                                b_or(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= b_or.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x7) [ # andi
                                                decode_i_type_unsigned(inst).inline(),
                                                b_and(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= b_and.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x1) [ # slli
                                                decode_i_type_signed(inst).inline(),
                                                b_shift_left(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= b_shift_left.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x5) [
                                                decode_i_type_signed(inst).inline(),
                                                locals.funct7 <= inst >> 25,
                                                If (locals.funct7 == 0x0) [ # srli
                                                                b_shift_right(regs[locals.rs1], locals.imm).inline(),
                                                                regs[locals.rd] <= b_shift_right.result,
                                                                StopThisScript()
                                                ],
                                                If (locals.funct7 == 0b0100000) [ # srai
                                                                b_shift_right_arith(regs[locals.rs1], locals.imm & 0x1f).inline(),
                                                                regs[locals.rd] <= b_shift_right_arith.result,
                                                                StopThisScript()
                                                ]
                                ],
                                If (locals.funct3 == 0x2) [ # slti
                                                decode_i_type_signed(inst).inline(),
                                                less_than_signed(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= less_than_signed.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x3) [ # sltiu
                                                decode_i_type_signed(inst).inline(),
                                                less_than_unsigned(regs[locals.rs1], locals.imm).inline(),
                                                regs[locals.rd] <= less_than_unsigned.result,
                                                StopThisScript()
                                ]
                ],
                If (locals.opcode == 0b1100011) [
                                decode_b_type(inst).inline(),
                                If (locals.funct3 == 0x0) [ # beq
                                                If (regs[locals.rs1] == regs[locals.rs2]) [
                                                                add(pc - 4, locals.imm).inline(),
                                                                pc <= add.result,
                                                                StopThisScript()
                                                ]
                                ],
                                If (locals.funct3 == 0x1) [ # bne
                                                If (regs[locals.rs1] != regs[locals.rs2]) [
                                                                add(pc - 4, locals.imm).inline(),
                                                                pc <= add.result,
                                                                StopThisScript()
                                                ]
                                ],
                                If (locals.funct3 == 0x6) [ # bltu
                                                If (regs[locals.rs1] < regs[locals.rs2]) [
                                                                add(pc - 4, locals.imm).inline(),
                                                                pc <= add.result,
                                                                StopThisScript()
                                                ]
                                ],
                                If (locals.funct3 == 0x7) [ # bgeu
                                                If ((regs[locals.rs1] < regs[locals.rs2]).NOT()) [
                                                                add(pc - 4, locals.imm).inline(),
                                                                pc <= add.result,
                                                                StopThisScript()
                                                ]
                                ],
                                If (locals.funct3 == 0x4) [ # blt
                                                toSigned32(regs[locals.rs1]).inline(),
                                                locals.srs1 <= toSigned32.result,
                                                toSigned32(regs[locals.rs2]).inline(),
                                                If (locals.srs1 < toSigned32.result) [
                                                                add(pc - 4, locals.imm).inline(),
                                                                pc <= add.result
                                                ],
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x5) [ # bge
                                                toSigned32(regs[locals.rs1]).inline(),
                                                locals.srs1 <= toSigned32.result,
                                                toSigned32(regs[locals.rs2]).inline(),
                                                If ((locals.srs1 < toSigned32.result).NOT()) [
                                                                add(pc - 4, locals.imm).inline(),
                                                                pc <= add.result
                                                ],
                                                StopThisScript()
                                ]
                ],
                If (locals.opcode == 0b0110011) [
                                decode_r_type(inst).inline(),
                                If (locals.funct3 == 0x0) [
                                                If (locals.funct7 == 0x0) [ # add
                                                                add(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                                regs[locals.rd] <= add.result,
                                                                StopThisScript()
                                                ],
                                                If (locals.funct7 == 0x20) [ # sub
                                                                sub(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                                regs[locals.rd] <= sub.result,
                                                                StopThisScript()
                                                ]
                                ],
                                If (locals.funct3 == 0x4) [ # xor
                                                b_xor(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= b_xor.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x6) [ # or
                                                b_or(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= b_or.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x7) [ # and
                                                b_and(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= b_and.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x1) [ # sll
                                                b_shift_left(regs[locals.rs1], regs[locals.rs2] & 0x1f).inline(),
                                                regs[locals.rd] <= b_shift_left.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x5) [
                                                If (locals.funct7 == 0x0) [ # srl
                                                                b_shift_right(regs[locals.rs1], regs[locals.rs2] & 0x1f).inline(),
                                                                regs[locals.rd] <= b_shift_right.result,
                                                                StopThisScript()
                                                ],
                                                If (locals.funct7 == 0x20) [ # sra
                                                                b_shift_right_arith(regs[locals.rs1], regs[locals.rs2] & 0x1f).inline(),
                                                                regs[locals.rd] <= b_shift_right_arith.result,
                                                                StopThisScript()
                                                ]
                                ],
                                If (locals.funct3 == 0x2) [ # slt
                                                less_than_signed(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= less_than_signed.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x3) [ # sltu
                                                less_than_unsigned(regs[locals.rs1], regs[locals.rs2]).inline(),
                                                regs[locals.rd] <= less_than_unsigned.result,
                                                StopThisScript()
                                ]
                ],
                If (locals.opcode == 0b0000011) [
                                decode_i_type(inst).inline(),
                                decode_i_type_signed(inst).inline(),
                                add(regs[locals.rs1], locals.imm).inline(),
                                locals.addr <= add.result,
                                If (locals.funct3 == 0x0) [ # lb
                                                bus_load8(locals.addr).inline(),
                                                toSigned8(bus_result).inline(),
                                                toUnsigned32(toSigned8.result).inline(),
                                                regs[locals.rd] <= toUnsigned32.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x1) [ # lh
                                                bus_load16(locals.addr).inline(),
                                                toSigned16(bus_result).inline(),
                                                toUnsigned32(toSigned16.result).inline(),
                                                regs[locals.rd] <= toUnsigned32.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x2) [ # lw
                                                bus_load32(locals.addr).inline(),
                                                regs[locals.rd] <= bus_result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x4) [ # lbu
                                                bus_load8(locals.addr).inline(),
                                                regs[locals.rd] <= bus_result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x5) [ # lhu
                                                bus_load16(locals.addr).inline(),
                                                regs[locals.rd] <= bus_result,
                                                StopThisScript()
                                ]
                ],
                If (locals.opcode == 0b0100011) [
                                decode_s_type(inst).inline(),
                                add(regs[locals.rs1], locals.imm).inline(),
                                locals.addr <= add.result,
                                If (locals.funct3 == 0x0) [ # sb
                                                bus_store8(locals.addr, regs[locals.rs2] & 0xff).inline(),
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x1) [ # sh
                                                bus_store16(locals.addr, regs[locals.rs2] & 0xffff).inline(),
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0x2) [ # sw
                                                bus_store32(locals.addr, regs[locals.rs2]).inline(),
                                                StopThisScript()
                                ]
                ],
                If (locals.opcode == 0b1101111) [ # jal
                                decode_j_type(inst).inline(),
                                add(pc - 4, locals.imm).inline(),
                                regs[locals.rd] <= pc,
                                pc <= add.result,
                                StopThisScript()
                ],
                If (locals.opcode == 0b1100111) [ # jalr
                                decode_i_type(inst).inline(),
                                decode_i_type_signed(inst).inline(),
                                add(regs[locals.rs1], locals.imm).inline(),
                                regs[locals.rd] <= pc,
                                pc <= (add.result >> 1) << 1,
                                StopThisScript()
                ],
                If (locals.opcode == 0b0110111) [ # lui
                                decode_u_type(inst).inline(),
                                regs[locals.rd] <= locals.imm,
                                StopThisScript()
                ],
                If (locals.opcode == 0b0010111) [ # auipc
                                decode_u_type(inst).inline(),
                                add(pc - 4, locals.imm).inline(),
                                regs[locals.rd] <= add.result,
                                StopThisScript()
                ],
                If (locals.opcode == 0b0001111) [ # fence
                                StopThisScript()
                ],
                If (locals.opcode == 0b1110011) [ # system
                                decode_i_type_csr(inst).inline(),
                                If (locals.funct3 == 0b001) [ # csrrw
                                                locals.csr_tmp <= csrs[locals.imm],
                                                csrs[locals.imm] <= regs[locals.rs1],
                                                regs[locals.rd] <= locals.csr_tmp,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0b010) [ # csrrs
                                                b_or(csrs[locals.imm], regs[locals.rs1]).inline(),
                                                regs[locals.rd] <= csrs[locals.imm],
                                                csrs[locals.imm] <= b_or.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0b011) [ # csrrc
                                                b_and(csrs[locals.imm], regs[locals.rs1]).inline(),
                                                regs[locals.rd] <= csrs[locals.imm],
                                                csrs[locals.imm] <= b_and.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0b101) [ # csrrwi
                                                locals.csr_tmp <= csrs[locals.imm],
                                                csrs[locals.imm] <= locals.rs1,
                                                regs[locals.rd] <= locals.csr_tmp,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0b110) [ # csrrsi
                                                b_or(csrs[locals.imm], locals.rs1).inline(),
                                                regs[locals.rd] <= csrs[locals.imm],
                                                csrs[locals.imm] <= b_or.result,
                                                StopThisScript()
                                ],
                                If (locals.funct3 == 0b111) [ # csrrci
                                                b_and(csrs[locals.imm], locals.rs1).inline(),
                                                regs[locals.rd] <= csrs[locals.imm],
                                                csrs[locals.imm] <= b_and.result,
                                                StopThisScript()
                                ],
                ],
                locals.matched <= 0
]

@emu.proc_def()
def to_hex (locals, value): return [
                locals.result <= hex_lut[(value >> 28) & 0xf]
                                .join(hex_lut[(value >> 24) & 0xf])
                                .join(hex_lut[(value >> 20) & 0xf])
                                .join(hex_lut[(value >> 16) & 0xf])
                                .join(hex_lut[(value >> 12) & 0xf])
                                .join(hex_lut[(value >> 8) & 0xf])
                                .join(hex_lut[(value >> 4) & 0xf])
                                .join(hex_lut[value & 0xf])
]

@emu.proc_def()
def breakpoint (locals): return [
                If (execute.matched == 0) [
                                uart.append("* Unknown instruction encountered")
                ],
                If (pc - DRAM_BASE > DRAM_SIZE) [
                                uart.append("* PC exceeded DRAM size"),
                                uart.append(Literal("PC: ").join(pc)),
                                uart.append(Literal("DRAM_BASE: ").join(DRAM_BASE)),
                                uart.append(Literal("DRAM_SIZE: ").join(DRAM_SIZE))
                ],
                If (pc == 0) [
                                uart.append("* Jump to null address")
                ],
                to_hex(pc),
                uart.append(Literal("PC: ").join(to_hex.result)),
                to_hex(locals.old_pc),
                uart.append(Literal("Prev PC: ").join(to_hex.result)),
                to_hex(bus_result),
                uart.append(Literal("inst: ").join(to_hex.result)),
                uart.append(Literal("tick: ").join(ticks)),
                uart.append("--- Registers ---"),
                locals.i[:regs.len():1] >> [
                                to_hex(regs[locals.i]),
                                uart.append(locals.i.join(": 0x").join(to_hex.result))
                ],
                StopAll()
]

@emu.proc_def()
def tick (locals): return [
                ticks <= ticks + 1,
                If ((pc - DRAM_BASE > DRAM_SIZE).OR(pc == 0)) [
                                breakpoint()   
                ],
                regs[0] <= 0,
                fetch(pc).inline(),
                breakpoint.old_pc <= pc,
                pc <= pc + 4,
                execute(bus_result).inline()
]

@emu.proc_def()
def loop (locals): return [
                Repeat (300000) [
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
