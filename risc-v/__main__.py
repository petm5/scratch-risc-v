from boiga import *
from math import floor, ceil

project = Project()

empty = project.new_sprite("empty")

emu = project.new_sprite("RISCV32")

DRAM_SIZE = 0x44000
DRAM_BASE = 0x80000000

CODE_MAX = 0x10000

VIDEO_BASE = 0x80040000

dram = emu.new_list("_DRAM", [0] * DRAM_SIZE)
jit = emu.new_list("_JIT_CACHE", [0] * 4 * CODE_MAX)
regs = emu.new_list("_REGS", [0] * 32)
# csrs = emu.new_list("_CSRS", [0] * 4096)
pc = emu.new_var("_PC")
ticks = emu.new_var("_TICKS")
jit_index = emu.new_var("_JIT_INDEX")
running = emu.new_var("_RUNNING")

code = emu.new_list("_CODE", open("_CODE.txt", "r").readlines())

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

base2_lut_contents = []
for a in range(2 ** 5):
                base2_lut_contents.append(2 ** a)

base2_lut = emu.new_list("_BASE2_LUT", base2_lut_contents)

# ascii = []
# for a in range(256):
#                 ascii.append(chr(a))

# ascii_lut = emu.new_list("_ASCII", ascii)

# hex_lut = emu.new_var("_HEXA", '0123456789abcdef')

@emu.proc_def(inline_only=True)
def clear_screen (locals): return [
                Show(),
                SetSize(100),
                SetCostume("bg"),
                SetXYPos(0, 0),
                Show(),
                Stamp(),
                SetSize(50),
                SetXYPos(-240, 180),
                # SetCostume("cursor")
                Hide(),
                
]

@emu.proc_def()
def reset (locals): return [
                clear_screen().inline(),
                # newlines <= 0,
                # h_lines <= 0,
                # history.delete_all(),
                locals.i[:DRAM_SIZE:1] >> [
                                dram[locals.i] <= 0
                ],
                locals.i[:CODE_MAX*4:1] >> [
                                jit[locals.i] <= 0
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
def toSigned32 (locals, int): return (int + 2147483648) % 4294967296 - 2147483648

@emu.proc_def(inline_only=True)
def toSigned16 (locals, int): return (int + 32768) % 65536 - 32768

@emu.proc_def(inline_only=True)
def toSigned8 (locals, int): return (int + 128) % 256 - 128

@emu.proc_def(inline_only=True)
def toUnsigned32 (locals, int): return (int + 4294967296) % 4294967296

### ALU operations

@emu.proc_def(inline_only=True)
def add (locals, a, b): return (a + b) & 0xffffffff

@emu.proc_def(inline_only=True)
def sub (locals, a, b): return (a - b) & 0xffffffff

@emu.proc_def(inline_only=True)
def multiply (locals, a, b): return toUnsigned32(a * b).inline() & 0xffffffff

@emu.proc_def(inline_only=True)
def multiply_upper (locals, a, b): return toUnsigned32((a * b) >> 32).inline()

@emu.proc_def(inline_only=True)
def divide (locals, a, b): return toUnsigned32(floor(a / b)).inline()

@emu.proc_def(inline_only=True)
def remainder (locals, a, b): return toUnsigned32((a % b)).inline()

@emu.proc_def(inline_only=True)
def b_xor (locals, a, b): return xor_lut[(a & 0xff) + ((b & 0xff) * 256)] + (xor_lut[((a >> 8) & 0xff) + (((b >> 8) & 0xff) * 256)] << 8) + (xor_lut[((a >> 16) & 0xff) + (((b >> 16) & 0xff) * 256)] << 16) + (xor_lut[((a >> 24) & 0xff) + (((b >> 24) & 0xff) * 256)] << 24)

@emu.proc_def(inline_only=True)
def b_or (locals, a, b): return or_lut[(a & 0xff) + ((b & 0xff) * 256)] + (or_lut[((a >> 8) & 0xff) + (((b >> 8) & 0xff) * 256)] << 8) + (or_lut[((a >> 16) & 0xff) + (((b >> 16) & 0xff) * 256)] << 16) + (or_lut[((a >> 24) & 0xff) + (((b >> 24) & 0xff) * 256)] << 24)

@emu.proc_def(inline_only=True)
def b_and (locals, a, b): return and_lut[(a & 0xff) + ((b & 0xff) * 256)] + (and_lut[((a >> 8) & 0xff) + (((b >> 8) & 0xff) * 256)] << 8) + (and_lut[((a >> 16) & 0xff) + (((b >> 16) & 0xff) * 256)] << 16) + (and_lut[((a >> 24) & 0xff) + (((b >> 24) & 0xff) * 256)] << 24)

@emu.proc_def(inline_only=True)
def b_shift_left (locals, a, b): return (a << b) & 0xffffffff

@emu.proc_def(inline_only=True)
def b_shift_right (locals, a, b): return floor(a / base2_lut[b])

@emu.proc_def(inline_only=True)
def b_shift_right_arith (locals, a, b): return toUnsigned32(floor(toSigned32(a).inline() / base2_lut[b])).inline()

@emu.proc_def(inline_only=True)
def less_than_unsigned (locals, a, b): return abs(a < b)

@emu.proc_def(inline_only=True)
def less_than_signed (locals, a, b): return abs(toSigned32(a).inline() < toSigned32(b).inline())

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
def hw_load8 (locals, addr): return [
                # If (addr == 0x10000000) [
                #                 bus_result <= input[0],
                #                 input.delete_at(0),
                #                 StopThisScript()
                # ],
                # If (addr == 0x10000005) [
                #                 bus_result <= abs(input.len() > 0),
                #                 StopThisScript()
                # ]
]

@emu.proc_def()
def bus_load32 (locals, addr): return [
                If (addr < DRAM_BASE) [
                                hw_load8(addr),
                ].Else [
                                mem_load32(addr - DRAM_BASE).inline()
                ]
]

@emu.proc_def()
def bus_load16 (locals, addr): return [
                If (addr < DRAM_BASE) [
                                hw_load8(addr),
                ].Else [
                                mem_load16(addr - DRAM_BASE).inline()
                ]
]

@emu.proc_def()
def bus_load8 (locals, addr): return [
                If (addr < DRAM_BASE) [
                                hw_load8(addr)
                ].Else [
                                mem_load8(addr - DRAM_BASE).inline()
                ]
]

@emu.proc_def()
def mem_store8 (locals, index, value): return [
                dram[index] <= value,
                jit[index*4] <= 0
]

@emu.proc_def()
def hw_store8 (locals, addr, value): return [
                If (addr == 0x10000000) [
                                uart.append(value),
                                StopThisScript()
                ]
]

@emu.proc_def()
def bus_store32 (locals, addr, value): return [
                If (addr < DRAM_BASE) [
                                hw_store8(addr, value & 0xff)
                ].Else [
                                mem_store8(addr - DRAM_BASE, value & 0xff),
                                mem_store8(addr - DRAM_BASE + 1, (value >> 8) & 0xff),
                                mem_store8(addr - DRAM_BASE + 2, (value >> 16) & 0xff),
                                mem_store8(addr - DRAM_BASE + 3, (value >> 24) & 0xff)
                ]
]

@emu.proc_def()
def bus_store16 (locals, addr, value): return [
                If (addr < DRAM_BASE) [
                                hw_store8(addr, value & 0xff)
                ].Else [
                                mem_store8(addr - DRAM_BASE, value & 0xff),
                                mem_store8(addr - DRAM_BASE + 1, (value >> 8) & 0xff)
                ]
]

@emu.proc_def()
def bus_store8 (locals, addr, value): return [
                If (addr < DRAM_BASE) [
                                hw_store8(addr, value & 0xff).inline()
                ].Else [
                                mem_store8(addr - DRAM_BASE, value & 0xff).inline()
                ]
]

@emu.proc_def(inline_only=True)
def fetch (locals, pc): return [
                locals.addr <= pc - DRAM_BASE,
                mem_load32(locals.addr).inline()
]

### Decoders

@emu.proc_def(inline_only=True)
def decode_r_type (locals, inst): return [
                jit[jit_index+1] <= (inst >> 7) & 0x1f, # rd
                jit[jit_index+2] <= (inst >> 15) & 0x1f, # rs1
                jit[jit_index+3] <= (inst >> 20) & 0x1f, # rs2
                locals.funct3 <= (inst >> 12) & 0x7,
                locals.funct7 <= inst >> 25
]

@emu.proc_def(inline_only=True)
def decode_i_type (locals, inst): return [
                jit[jit_index+1] <= (inst >> 7) & 0x1f, # rd
                jit[jit_index+2] <= (inst >> 15) & 0x1f, # rs1
                locals.funct3 <= (inst >> 12) & 0x7
]

@emu.proc_def(inline_only=True)
def decode_i_type_signed (locals, inst): return [
                jit[jit_index+3] <= toSigned32(inst).inline() >> 20 # imm
]

@emu.proc_def(inline_only=True)
def decode_i_type_signed_shift (locals, inst): return [
                jit[jit_index+3] <= (toSigned32(inst).inline() >> 20) & 0x1f, # imm
                locals.funct7 <= inst >> 25,
]

@emu.proc_def(inline_only=True)
def decode_i_type_unsigned (locals, inst): return [
                jit[jit_index+3] <= toUnsigned32(toSigned32(inst).inline() >> 20).inline() # imm
]

# @emu.proc_def(inline_only=True)
# def decode_i_type_csr (locals, inst): return [
#                 jit[jit_index+1] <= (inst >> 7) & 0x1f, # rd
#                 jit[jit_index+2] <= (inst >> 15) & 0x1f, # rs1
#                 locals.funct3 <= (inst >> 12) & 0x7,
#                 jit[jit_index+3] <= inst >> 20 # imm
# ]

@emu.proc_def(inline_only=True)
def decode_s_type (locals, inst): return [
                jit[jit_index+1] <= (inst >> 15) & 0x1f, # rs1
                jit[jit_index+2] <= (inst >> 20) & 0x1f, # rs2
                locals.funct3 <= (inst >> 12) & 0x7,
                jit[jit_index+3] <= (toUnsigned32(toSigned32(inst).inline() >> 25).inline() << 5) + ((inst >> 7) & 0x1f) # imm
]

@emu.proc_def(inline_only=True)
def decode_b_type (locals, inst): return [
                jit[jit_index+1] <= (inst >> 15) & 0x1f, # rs1
                jit[jit_index+2] <= (inst >> 20) & 0x1f, # rs2
                locals.funct3 <= (inst >> 12) & 0x7,
                jit[jit_index+3] <= ((toSigned32(inst).inline() >> 31) << 12) + (((inst >> 7) & 0x01) << 11) + (((inst >> 25) & 0x3f) << 5) + (((inst >> 8) & 0x0f) << 1) # imm
]

@emu.proc_def(inline_only=True)
def decode_u_type (locals, inst): return [
                jit[jit_index+1] <= (inst >> 7) & 0x1f, # rd
                jit[jit_index+3] <= (inst >> 12) << 12 # imm
]

@emu.proc_def(inline_only=True)
def decode_j_type (locals, inst): return [
                jit[jit_index+1] <= (inst >> 7) & 0x1f, # rd
                jit[jit_index+3] <= ((toSigned32(inst).inline() >> 31) << 20) + (((inst >> 12) & 0xff) << 12) + (((inst >> 20) & 0x01) << 11) + (((inst >> 21) & 0x03ff) << 1) # imm
]

@emu.proc_def()
def jit_compile (locals, inst): return [
                locals.opcode <= inst & 0x7f,
                If (locals.opcode == 0b0010011) [
                                decode_i_type(inst).inline(),
                                If (decode_i_type.funct3 == 0x0) [ # addi
                                                decode_i_type_signed(inst).inline(),
                                                jit[jit_index] <= 1,
                                                StopThisScript()
                                ],
                                If (decode_i_type.funct3 == 0x4) [ # xori
                                                decode_i_type_unsigned(inst).inline(),
                                                jit[jit_index] <= 2,
                                                StopThisScript()
                                ],
                                If (decode_i_type.funct3 == 0x6) [ # ori
                                                decode_i_type_unsigned(inst).inline(),
                                                jit[jit_index] <= 3,
                                                StopThisScript()
                                ],
                                If (decode_i_type.funct3 == 0x7) [ # andi
                                                decode_i_type_unsigned(inst).inline(),
                                                jit[jit_index] <= 4,
                                                StopThisScript()
                                ],
                                If (decode_i_type.funct3 == 0x1) [ # slli
                                                decode_i_type_signed(inst).inline(),
                                                jit[jit_index] <= 5,
                                                StopThisScript()
                                ],
                                If (decode_i_type.funct3 == 0x5) [
                                                decode_i_type_signed_shift(inst).inline(),
                                                If (decode_i_type_signed_shift.funct7 == 0x0) [ # srli
                                                                jit[jit_index] <= 6,
                                                                StopThisScript()
                                                ],
                                                If (decode_i_type_signed_shift.funct7 == 0b0100000) [ # srai
                                                                jit[jit_index] <= 7,
                                                                StopThisScript()
                                                ]
                                ],
                                If (decode_i_type.funct3 == 0x2) [ # slti
                                                decode_i_type_signed(inst).inline(),
                                                jit[jit_index] <= 8,
                                                StopThisScript()
                                ],
                                If (decode_i_type.funct3 == 0x3) [ # sltiu
                                                decode_i_type_unsigned(inst).inline(),
                                                jit[jit_index] <= 9,
                                                StopThisScript()
                                ]
                ],
                If (locals.opcode == 0b1100011) [
                                decode_b_type(inst).inline(),
                                If (decode_b_type.funct3 == 0x0) [ # beq
                                                jit[jit_index] <= 10,
                                                StopThisScript()
                                ],
                                If (decode_b_type.funct3 == 0x1) [ # bne
                                                jit[jit_index] <= 11,
                                                StopThisScript()
                                ],
                                If (decode_b_type.funct3 == 0x6) [ # bltu
                                                jit[jit_index] <= 12,
                                                StopThisScript()
                                ],
                                If (decode_b_type.funct3 == 0x7) [ # bgeu
                                                jit[jit_index] <= 13,
                                                StopThisScript()
                                ],
                                If (decode_b_type.funct3 == 0x4) [ # blt
                                                jit[jit_index] <= 14,
                                                StopThisScript()
                                ],
                                If (decode_b_type.funct3 == 0x5) [ # bge
                                                jit[jit_index] <= 15,
                                                StopThisScript()
                                ]
                ],
                If (locals.opcode == 0b0110011) [
                                decode_r_type(inst).inline(),
                                If (decode_r_type.funct7 == 0b0) [
                                                If (decode_r_type.funct3 == 0x0) [ # add
                                                                jit[jit_index] <= 16,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x4) [ # xor
                                                                jit[jit_index] <= 18,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x6) [ # or
                                                                jit[jit_index] <= 19,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x7) [ # and
                                                                jit[jit_index] <= 20,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x1) [ # sll
                                                                jit[jit_index] <= 21,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x5) [ # srl
                                                                jit[jit_index] <= 22,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x2) [ # slt
                                                                jit[jit_index] <= 24,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x3) [ # sltu
                                                                jit[jit_index] <= 25,
                                                                StopThisScript()
                                                ]
                                ],
                                If (decode_r_type.funct7 == 0x20) [
                                                If (decode_r_type.funct3 == 0x0) [ # sub
                                                                jit[jit_index] <= 17,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x5) [ # sra
                                                                jit[jit_index] <= 23,
                                                                StopThisScript()
                                                ]
                                ],
                                If (decode_r_type.funct7 == 0x1) [
                                                If (decode_r_type.funct3 == 0x0) [ # mul
                                                                jit[jit_index] <= 40,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x1) [ # mulh
                                                                jit[jit_index] <= 41,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x2) [ # mulhu
                                                                jit[jit_index] <= 42,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x3) [ # mulhsu
                                                                jit[jit_index] <= 43,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x4) [ # div
                                                                jit[jit_index] <= 44,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x5) [ # divu
                                                                jit[jit_index] <= 45,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x6) [ # rem
                                                                jit[jit_index] <= 46,
                                                                StopThisScript()
                                                ],
                                                If (decode_r_type.funct3 == 0x7) [ # remu
                                                                jit[jit_index] <= 47,
                                                                StopThisScript()
                                                ]
                                ]
                ],
                If (locals.opcode == 0b0000011) [
                                decode_i_type(inst).inline(),
                                decode_i_type_signed(inst).inline(),
                                If (decode_i_type.funct3 == 0x0) [ # lb
                                                jit[jit_index] <= 26,
                                                StopThisScript()
                                ],
                                If (decode_i_type.funct3 == 0x1) [ # lh
                                                jit[jit_index] <= 27,
                                                StopThisScript()
                                ],
                                If (decode_i_type.funct3 == 0x2) [ # lw
                                                jit[jit_index] <= 28,
                                                StopThisScript()
                                ],
                                If (decode_i_type.funct3 == 0x4) [ # lbu
                                                jit[jit_index] <= 29,
                                                StopThisScript()
                                ],
                                If (decode_i_type.funct3 == 0x5) [ # lhu
                                                jit[jit_index] <= 30,
                                                StopThisScript()
                                ]
                ],
                If (locals.opcode == 0b0100011) [
                                decode_s_type(inst).inline(),
                                If (decode_s_type.funct3 == 0x0) [ # sb
                                                jit[jit_index] <= 31,
                                                StopThisScript()
                                ],
                                If (decode_s_type.funct3 == 0x1) [ # sh
                                                jit[jit_index] <= 32,
                                                StopThisScript()
                                ],
                                If (decode_s_type.funct3 == 0x2) [ # sw
                                                jit[jit_index] <= 33,
                                                StopThisScript()
                                ]
                ],
                If (locals.opcode == 0b1101111) [ # jal
                                decode_j_type(inst).inline(),
                                jit[jit_index] <= 34,
                                StopThisScript()
                ],
                If (locals.opcode == 0b1100111) [ # jalr
                                decode_i_type(inst).inline(),
                                decode_i_type_signed(inst).inline(),
                                jit[jit_index] <= 35,
                                StopThisScript()
                ],
                If (locals.opcode == 0b0110111) [ # lui
                                decode_u_type(inst).inline(),
                                jit[jit_index] <= 36,
                                StopThisScript()
                ],
                If (locals.opcode == 0b0010111) [ # auipc
                                decode_u_type(inst).inline(),
                                jit[jit_index] <= 37,
                                StopThisScript()
                ],
                # If (locals.opcode == 0b0001111) [ # fence = nop
                #                 jit[jit_index] <= 39,
                #                 StopThisScript()
                # ],
                If (locals.opcode == 0b1110011) [ # system
                                jit[jit_index] <= 38,
                                StopThisScript()
                ],
                jit[jit_index] <= 39
]

temp = emu.new_var("_TEMP")

execute_instrs = {
                6: [ # srli
                                regs[jit[jit_index+1]] <= b_shift_right(regs[jit[jit_index+2]], jit[jit_index+3]).inline(),
                ],
                7: [ # srai
                                regs[jit[jit_index+1]] <= b_shift_right_arith(regs[jit[jit_index+2]], jit[jit_index+3] & 0x1f).inline(),
                ],
                8: [ # slti
                                regs[jit[jit_index+1]] <= less_than_signed(regs[jit[jit_index+2]], jit[jit_index+3]).inline(),
                ],
                12: [ # bltu
                                If (regs[jit[jit_index+1]] < regs[jit[jit_index+2]]) [
                                                pc <= add(pc - 4, jit[jit_index+3]).inline()
                                ],
                ],
                37: [ # auipc
                                regs[jit[jit_index+1]] <= add(pc - 4, jit[jit_index+3]).inline(),
                ],
                40: [ # mul
                                regs[jit[jit_index+1]] <= multiply(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                41: [ # mulh
                                regs[jit[jit_index+1]] <= multiply_upper(toSigned32(regs[jit[jit_index+2]]).inline(), toSigned32(regs[jit[jit_index+3]]).inline()).inline(),
                ],
                42: [ # mulhu
                                regs[jit[jit_index+1]] <= multiply_upper(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                43: [ # mulhisu
                                regs[jit[jit_index+1]] <= multiply_upper(toSigned32(regs[jit[jit_index+2]]).inline(), regs[jit[jit_index+3]]).inline()
                ],
                44: [ # div
                                regs[jit[jit_index+1]] <= divide(toSigned32(regs[jit[jit_index+2]]).inline(), toSigned32(regs[jit[jit_index+3]]).inline()).inline(),
                ],
                45: [ # divu
                                regs[jit[jit_index+1]] <= divide(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                46: [ # rem
                                regs[jit[jit_index+1]] <= remainder(toSigned32(regs[jit[jit_index+2]]).inline(), toSigned32(regs[jit[jit_index+3]]).inline()).inline(),
                ],
                47: [ # remu
                                regs[jit[jit_index+1]] <= remainder(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                34: [ # jal
                                temp <= add(pc - 4, jit[jit_index+3]).inline(),
                                regs[jit[jit_index+1]] <= pc,
                                pc <= temp,
                ],
                35: [ # jalr
                                temp <= (add(regs[jit[jit_index+2]], jit[jit_index+3]).inline() >> 1) << 1,
                                regs[jit[jit_index+1]] <= pc,
                                pc <= temp,
                ],
                36: [ # lui
                                regs[jit[jit_index+1]] <= jit[jit_index+3],
                ],
                1: [ # addi
                                regs[jit[jit_index+1]] <= add(regs[jit[jit_index+2]], jit[jit_index+3]).inline(),
                ],
                2: [ # xori
                                regs[jit[jit_index+1]] <= b_xor(regs[jit[jit_index+2]], jit[jit_index+3]).inline(),
                ],
                3: [ # ori
                                regs[jit[jit_index+1]] <= b_or(regs[jit[jit_index+2]], jit[jit_index+3]).inline(),
                ],
                4: [ # andi
                                regs[jit[jit_index+1]] <= b_and(regs[jit[jit_index+2]], jit[jit_index+3]).inline(),
                ],
                5: [ # slli
                                regs[jit[jit_index+1]] <= b_shift_left(regs[jit[jit_index+2]], jit[jit_index+3]).inline(),
                ],
                9: [ # sltiu
                                regs[jit[jit_index+1]] <= less_than_unsigned(regs[jit[jit_index+2]], jit[jit_index+3]).inline(),
                ],
                10: [ # beq
                                If (regs[jit[jit_index+1]] == regs[jit[jit_index+2]]) [
                                                pc <= add(pc - 4, jit[jit_index+3]).inline(),
                                ],
                ],
                11: [ # bne
                                If (regs[jit[jit_index+1]] != regs[jit[jit_index+2]]) [
                                                pc <= add(pc - 4, jit[jit_index+3]).inline()
                                ],
                ],
                13: [ # bgeu
                                If ((regs[jit[jit_index+1]] < regs[jit[jit_index+2]]).NOT()) [
                                                pc <= add(pc - 4, jit[jit_index+3]).inline()
                                ],
                ],
                14: [ # blt
                                If (toSigned32(regs[jit[jit_index+1]]).inline() < toSigned32(regs[jit[jit_index+2]]).inline()) [
                                                pc <= add(pc - 4, jit[jit_index+3]).inline()
                                ],
                ],
                15: [ # bge
                                If ((toSigned32(regs[jit[jit_index+1]]).inline() < toSigned32(regs[jit[jit_index+2]]).inline()).NOT()) [
                                                pc <= add(pc - 4, jit[jit_index+3]).inline()
                                ],
                ],
                16: [ # add
                                regs[jit[jit_index+1]] <= add(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                17: [ # sub
                                regs[jit[jit_index+1]] <= sub(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                18: [ # xor
                                regs[jit[jit_index+1]] <= b_xor(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                19: [ # or
                                regs[jit[jit_index+1]] <= b_or(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                20: [ # and
                                regs[jit[jit_index+1]] <= b_and(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                21: [ # sll
                                regs[jit[jit_index+1]] <= b_shift_left(regs[jit[jit_index+2]], regs[jit[jit_index+3]] & 0x1f).inline(),
                ],
                22: [ # srl
                                regs[jit[jit_index+1]] <= b_shift_right(regs[jit[jit_index+2]], regs[jit[jit_index+3]] & 0x1f).inline(),
                ],
                23: [ # sra
                                regs[jit[jit_index+1]] <= b_shift_right_arith(regs[jit[jit_index+2]], regs[jit[jit_index+3]] & 0x1f).inline(),
                ],
                24: [ # slt
                                regs[jit[jit_index+1]] <= less_than_signed(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                25: [ # sltu
                                regs[jit[jit_index+1]] <= less_than_unsigned(regs[jit[jit_index+2]], regs[jit[jit_index+3]]).inline(),
                ],
                26: [ # lb
                                bus_load8(add(regs[jit[jit_index+2]], jit[jit_index+3]).inline()),
                                regs[jit[jit_index+1]] <= toUnsigned32(toSigned8(bus_result).inline()).inline(),
                ],
                27: [ # lh
                                bus_load16(add(regs[jit[jit_index+2]], jit[jit_index+3]).inline()),
                                regs[jit[jit_index+1]] <= toUnsigned32(toSigned16(bus_result).inline()).inline(),
                ],
                28: [ # lw
                                bus_load32(add(regs[jit[jit_index+2]], jit[jit_index+3]).inline()),
                                regs[jit[jit_index+1]] <= bus_result,
                ],
                29: [ # lbu
                                bus_load8(add(regs[jit[jit_index+2]], jit[jit_index+3]).inline()),
                                regs[jit[jit_index+1]] <= bus_result,
                ],
                30: [ # lhu
                                bus_load16(add(regs[jit[jit_index+2]], jit[jit_index+3]).inline()),
                                regs[jit[jit_index+1]] <= bus_result,
                ],
                31: [ # sb
                                bus_store8(add(regs[jit[jit_index+1]], jit[jit_index+3]).inline(), regs[jit[jit_index+2]]),
                ],
                32: [ # sh
                                bus_store16(add(regs[jit[jit_index+1]], jit[jit_index+3]).inline(), regs[jit[jit_index+2]]),
                ],
                33: [ # sw
                                bus_store32(add(regs[jit[jit_index+1]], jit[jit_index+3]).inline(), regs[jit[jit_index+2]]),
                ],
                38: [
                                running <= 0,
                ]
}

@emu.proc_def(inline_only=True)
def execute (locals): return [
                locals.inst <= jit[jit_index],
                compile_match(locals.inst, execute_instrs)
]

# Compile a match operation from a dictionary mapping cases to instructions
# Makes use of a binary search to improve performance (worst-case complexity: O(log n) rather than O(n))
def compile_match (value, instrs_dict):
                return compile_match_recurse(value, list(sorted(instrs_dict.items())))

def compile_match_recurse (value, instrs):
                length = len(instrs)
                if length > 1:
                                half_length = floor(length / 2)
                                search_inst = instrs[half_length][0] - 1
                                return If ((value > search_inst), [compile_match_recurse(value, instrs[half_length:])]).Else [compile_match_recurse(value, instrs[:half_length])]
                elif length == 1:
                                return instrs[0][1]

@emu.proc_def()
def tick (locals): return [
                ticks <= ticks + 1,
                regs[0] <= 0,
                jit_index <= (pc - DRAM_BASE) * 4,
                If (jit[jit_index] == 0) [
                                fetch(pc).inline(),
                                jit_compile(bus_result),
                ],
                pc <= pc + 4,
                execute().inline(),
]

@emu.proc_def(inline_only=True)
def draw (locals): return [
                locals.index <= VIDEO_BASE - DRAM_BASE,
                locals.y <= 180 - 4,
                SetPenSize(11),
                Show(),
                Repeat (48) [
                                locals.x <= -240 + 4,
                                Repeat (64) [
                                                SetXYPos(locals.x, locals.y),
                                                SetPenColour(dram[locals.index] * 65536 + dram[locals.index+1] * 256 + dram[locals.index+2]),
                                                PenDown(),
                                                PenUp(),
                                                locals.index <= locals.index + 4,
                                                locals.x <= locals.x + 7.5,
                                ],
                                locals.y <= locals.y - 7.5,
                ],
                Hide()
]

@emu.proc_def()
def loop (locals): return [
                running <= 1,
                Repeat (100000) [
                                tick(),
                                If (running == 0) [
                                                draw().inline(),
                                                StopThisScript()
                                ]
                ]
]

emu.on_flag([
            reset(),
            Forever [
            loop()
            ]
])

from io import BytesIO

emu.add_costume("bg", BytesIO(b'<svg width="480" height="360"><rect width="100%" height="100%" fill="black" stroke="black"/></svg>').getvalue(), "svg", (240, 180))

project.save("out/risc-v.sb3")
