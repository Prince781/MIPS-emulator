
#include <stdbool.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include "Instruction.h"
#include "RegFile.h"
#include "elf_reader/elf_reader.h"
#include "Syscall.h"

// opcodes
enum {
    SPECIAL = 0,
    REGIMM  = 1,
    ADDI    = 0b1000,
    ADDIU   = 0b1001,
    ANDI    = 0b1100,
    XORI    = 0b1110,
    ORI     = 0b1101,
    SLTI    = 0b1010,
    SLTIU   = 0b1011,
    BEQ     = 0b100,
    BEQL    = 0b10100,
    BGTZ    = 0b111,
    BLEZ    = 0b110,
    BLEZL   = 0b10110,
    BNE     = 0b101,
    BNEL    = 0b10101,
    LB      = 0b100000,
    LBU     = 0b100100,
    LH      = 0b100001,
    LHU     = 0b100101,
    LUI     = 0b1111,
    LW      = 0b100011,
    LWL     = 0b100010,
    LWR     = 0b100110,
    SB      = 0b101000,
    SH      = 0b101001,
    SW      = 0b101011,
    SWL     = 0b101010,
    SWR     = 0b101110,
    J       = 0b10,
    JAL     = 0b11
};

// functions
enum {
    ADD     = 0b100000,
    ADDU    = 0b100001,
    SUB     = 0b100010,
    SUBU    = 0b100011,
    DIV     = 0b11010,
    DIVU    = 0b11011,
    MULT    = 0b11000,
    MULTU   = 0b11001,
    MFHI    = 0b10000,
    MFLO    = 0b10010,
    MTHI    = 0b10001,
    MTLO    = 0b10011,
    AND     = 0b100100,
    XOR     = 0b100110,
    NOR     = 0b100111,
    OR      = 0b100101,
    SLL     = 0,
    SLLV    = 0b100,
    SLT     = 0b101010,
    SLTU    = 0b101011,
    SRA     = 0b11,
    SRAV    = 0b111,
    SRL     = 0b10,
    SRLV    = 0b110,
    JALR    = 0b1001,
    JR      = 0b1000,
    SYSCALL = 0b1100
};

// branch functions
enum {
    BGEZ    = 1,
    BGEZAL  = 0b10001,
    BLTZ    = 0,
    BLTZAL  = 0b10000
};

// alu operations
enum {
    ALUOP_NOP,
    ALUOP_GET1,
    ALUOP_GET2,
    ALUOP_ADD,
    ALUOP_SUB,
    ALUOP_DIV,
    ALUOP_DIVU,
    ALUOP_MULT,
    ALUOP_MULTU,
    ALUOP_MTHI,
    ALUOP_MTLO,
    ALUOP_AND,
    ALUOP_XOR,
    ALUOP_NOR,
    ALUOP_OR,
    ALUOP_SLT,
    ALUOP_SLTU,
    ALUOP_SLE,
    ALUOP_SLL,
    ALUOP_SRA,
    ALUOP_SRL,
    ALUOP_LUI
};

// opcode != 0 && opcode != 1
const instr Iinstructions[] = {
    [ADDI]      = { "    addi", .control=0b0001011000000000, ALUOP_ADD   },
    [ADDIU]     = { "   addiu", .control=0b0001011000000000, ALUOP_ADD   },
    [ANDI]      = { "    andi", .control=0b0001011000000000, ALUOP_AND   },
    [XORI]      = { "    xori", .control=0b0111011000000000, ALUOP_XOR   },
    [ORI]       = { "     ori", .control=0b0111011000000000, ALUOP_OR    },
    [SLTI]      = { "    slti", .control=0b0001011000000000, ALUOP_SLT   },
    [SLTIU]     = { "   sltiu", .control=0b0111011000000000, ALUOP_SLTU  },
    [BEQ]       = { "     beq", .control=0b0000100000000000, ALUOP_SUB   },
    [BEQL]      = { "    beql", .control=0b0000100000000010, ALUOP_SUB   },
    [BGTZ]      = { "    bgtz", .control=0b0000100000000000, ALUOP_SLE   },
    [BLEZ]      = { "    blez", .control=0b0000100000000001, ALUOP_SLE   },
    [BLEZL]     = { "   blezl", .control=0b0000100000000011, ALUOP_SLE   },
    [BNE]       = { "     bne", .control=0b0000100000000001, ALUOP_XOR   },
    [BNEL]      = { "    bnel", .control=0b0000100000000011, ALUOP_XOR   },
    [J]         = { "       j", .control=0b0011110000000000, ALUOP_NOP   },
    [JAL]       = { "     jal", .control=0b0011110000000000, ALUOP_NOP   },
    [LB]        = { "      lb", .control=0b0001011011001000, ALUOP_ADD   },
    [LBU]       = { "     lbu", .control=0b0001011011000000, ALUOP_ADD   },
    [LH]        = { "      lh", .control=0b0001011011011000, ALUOP_ADD   },
    [LHU]       = { "     lhu", .control=0b0001011011010000, ALUOP_ADD   },
    [LUI]       = { "     lui", .control=0b0001011000000000, ALUOP_LUI   },
    [LW]        = { "      lw", .control=0b0001011011100000, ALUOP_ADD   },
    [LWL]       = { "     lwl", .control=0b0001011011110100, ALUOP_ADD   },
    [LWR]       = { "     lwr", .control=0b0001011011110000, ALUOP_ADD   },
    [SB]        = { "      sb", .control=0b0001010100000000, ALUOP_ADD   },
    [SH]        = { "      sh", .control=0b0001010100010000, ALUOP_ADD   },
    [SW]        = { "      sw", .control=0b0001010100100000, ALUOP_ADD   },
    [SWL]       = { "     swl", .control=0b0001010100110100, ALUOP_ADD   },
    [SWR]       = { "     swr", .control=0b0001010100110000, ALUOP_ADD   }
};

// opcode == 0
const instr Rinstructions[] = {
    [ADD]       = { "     add", .control=0b0000001000000000, ALUOP_ADD   },
    [ADDU]      = { "    addu", .control=0b0000001000000000, ALUOP_ADD   },
    [SUB]       = { "     sub", .control=0b0000001000000000, ALUOP_SUB   },
    [SUBU]      = { "    subu", .control=0b0000001000000000, ALUOP_SUB   },
    [DIV]       = { "     div", .control=0b0000000000000000, ALUOP_DIV   },
    [DIVU]      = { "    divu", .control=0b0000000000000000, ALUOP_DIV   },
    [MULT]      = { "    mult", .control=0b0000000000000000, ALUOP_MULT  },
    [MULTU]     = { "   multu", .control=0b0000000000000000, ALUOP_MULTU },
    [MFHI]      = { "    mfhi", .control=0b0100001000000000, ALUOP_GET2  },
    [MFLO]      = { "    mflo", .control=0b0101001000000000, ALUOP_GET2  },
    [MTHI]      = { "    mthi", .control=0b0000000000000000, ALUOP_MTHI  },
    [MTLO]      = { "    mtlo", .control=0b0000000000000000, ALUOP_MTLO  },
    [AND]       = { "     and", .control=0b0000001000000000, ALUOP_AND   },
    [XOR]       = { "     xor", .control=0b0000001000000000, ALUOP_XOR   },
    [NOR]       = { "     nor", .control=0b0000001000000000, ALUOP_NOR   },
    [OR]        = { "      or", .control=0b0000001000000000, ALUOP_OR    },
    [SLL]       = { "     sll", .control=0b0010001000000000, ALUOP_SLL   },
    [SLLV]      = { "    sllv", .control=0b0110001000000000, ALUOP_SLL   },
    [SLT]       = { "     slt", .control=0b0000001000000000, ALUOP_SLT   },
    [SLTU]      = { "    sltu", .control=0b0000001000000000, ALUOP_SLTU  },
    [SRA]       = { "     sra", .control=0b0010001000000000, ALUOP_SRA   },
    [SRAV]      = { "    srav", .control=0b0110001000000000, ALUOP_SRA   },
    [SRL]       = { "     srl", .control=0b0010001000000000, ALUOP_SRL   },
    [SRLV]      = { "    srlv", .control=0b0110001000000000, ALUOP_SRL   },
    [JALR]      = { "    jalr", .control=0b0000110000000000, ALUOP_NOP   },
    [JR]        = { "      jr", .control=0b0000110000000000, ALUOP_NOP   },
    [SYSCALL]   = { " syscall", .control=0b0000000000000000, ALUOP_NOP   }
};

// opcode == 1
const instr Binstructions[] = {
    [BGEZ]      = { "    bgez", .control=0b1000100000000000, ALUOP_SLT   },
    [BGEZAL]    = { "  bgezal", .control=0b1000100000000000, ALUOP_SLT   },
    [BLTZ]      = { "    bltz", .control=0b1000100000000001, ALUOP_SLT   },
    [BLTZAL]    = { "  bltzal", .control=0b1000100000000001, ALUOP_SLT   }
};

const instr nop= { "     nop", .control=0b0000000000000000, ALUOP_NOP   };

uint32_t CurrentInstruction;
uint32_t pc;
static const instr *i;
static uint8_t opcode;
static uint8_t rs, rt, rd, shamt;
static uint16_t imm;
static uint32_t instr_index;
static uint8_t func;

static int32_t ALUresult;
static bool zero;

static int32_t memResult;

bool inDelaySlot = false;

struct delay_info delayInfo;

bool debug_memory = true;

void InstructionFetch(uint32_t PC) {
    if (inDelaySlot && delayInfo.delay_slot == PC
        && !delayInfo.condition && delayInfo.nullify) {
        printf("nullifying instruction at %08x\n", PC);
        writeWord(PC, 0, debug_memory);
    }
    
    CurrentInstruction = readWord(PC, debug_memory);
    pc = PC;
}

void InstructionDecode(void) {
    opcode = CurrentInstruction >> 26;
    rs = (CurrentInstruction >> 21) & 0x1Fu;
    rt = (CurrentInstruction >> 16) & 0x1Fu;
    rd = (CurrentInstruction >> 11) & 0x1Fu;
    shamt = (CurrentInstruction >> 6) & 0x1Fu;
    func = CurrentInstruction & 0x3Fu;
    
    imm = CurrentInstruction & 0xFFFFu;
    instr_index = CurrentInstruction & 0x03FFFFFFu;
    
    if (CurrentInstruction == 0)
        i = &nop;
    else if (opcode == SPECIAL)
        i = &Rinstructions[func];
    else if (opcode == REGIMM)
        i = &Binstructions[rt];
    else
        i = &Iinstructions[opcode];
}

void InstructionExecute(void) {
    static int64_t HILO;    // special register
    uint64_t *uHILO = (uint64_t *)&HILO;
    int32_t source;
    int32_t param;
    
    // alu stuff
    switch (i->aluSrc) {
    case ALUSRC_RT:
        source = RegFile[rs];
        param = RegFile[rt];
        break;
    case ALUSRC_IMM:
        source = RegFile[rs];
        param = *(int16_t *)&imm;   // sign-extend
        break;
    case ALUSRC_SHAMT:
        source = RegFile[rt];
        param = shamt;
        break;
    case ALUSRC_INSTR_INDEX:
        source = 0; // no source
        param = instr_index;    // lower 26 bits
        break;
    case ALUSRC_HI:
        source = 0; // no source
        param = (*uHILO) >> 32;
        break;
    case ALUSRC_LO:
        source = 0; // no source
        param = (*uHILO) & 0xFFFFFFFFu;
        break;
    case ALUSRC_RS:
        source = RegFile[rt];
        param = RegFile[rs];
        break;
    case ALUSRC_IMMU:
        source = RegFile[rs];
        param = imm;    // zero-extend
        break;
    case ALUSRC_REGIMM:
        source = RegFile[rs];
        param = 0;      // no param
        break;
    default:
        printf("unknown source\n");
        break;
    }
    
    int32_t result = 0;
    
    uint32_t usource = *(uint32_t*)&source;
    uint32_t uparam = *(uint32_t*)&param;
    
    switch (i->aluOp) {
    case ALUOP_ADD: // add
        result = source + param;
        break;
    case ALUOP_SUB: // sub
        result = source - param;
        break;
    case ALUOP_DIV: { // div
        div_t d = div(source, param);
        uint64_t quot = *(uint32_t*)&d.quot;
        uint64_t rem = *(uint32_t*)&d.rem;
        *uHILO = (rem << 32) | quot;
        break;}
    case ALUOP_DIVU: { // div (unsigned)
        uint64_t quot = usource / uparam;
        uint64_t rem = usource % uparam;
        *uHILO = (rem << 32) | quot;
        break;}
    case ALUOP_MULT: // mult
        HILO = source * param;
        break;
    case ALUOP_MULTU:
        *uHILO = usource * uparam;
        break;
    case ALUOP_MTHI:
        *uHILO = (((uint64_t)usource) << 32) | ((*uHILO) & 0xFFFFFFFFu);
        break;
    case ALUOP_MTLO:
        *uHILO = ((*uHILO) & 0xFFFFFFFF00000000ul) | usource;
        break;
    case ALUOP_AND:
        result = usource & uparam;
        break;
    case ALUOP_XOR:
        result = usource ^ uparam;
        break;
    case ALUOP_NOR:
        result = ~(usource | uparam);
        break;
    case ALUOP_OR:
        result = usource | uparam;
        break;
    case ALUOP_SLT:
        result = source < param;
        break;
    case ALUOP_SLTU:
        result = usource < uparam;
        break;
    case ALUOP_SLE:
        result = source <= param;
        break;
    case ALUOP_SLL:
        result = usource << uparam;
        break;
    case ALUOP_SRA:
        result = source >> param;
        break;
    case ALUOP_SRL:
        result = usource >> uparam;
        break;
    case ALUOP_LUI:
        result = uparam << 16;
        break;
    case ALUOP_GET1:
        result = source;
        break;
    case ALUOP_GET2:
        result = param;
        break;
    default:
    case ALUOP_NOP: // nop
        break;
    }
    
    RegFile[32] = (*uHILO) >> 32;
    RegFile[33] = (*uHILO) & 0xFFFFFFFFu;
    
    ALUresult = result;
    zero = ALUresult == 0;
}

void MemoryStage(uint32_t PC, uint32_t newPC) {
    if (i->memWrite) {
        uint32_t addr = ALUresult;
        uint32_t data = RegFile[rt];
        
        if (i->memType == 0b00) { // byte
            writeByte(addr, (uint8_t)(data & 0xFFu), debug_memory);
        } else if (i->memType == 0b01) { // halfword
            uint8_t temp = data;
            writeByte(addr+1, temp, debug_memory);
            temp = data >> 8;
            writeByte(addr+0, temp, debug_memory);
        } else if (i->memType == 0b10) { // word
            writeWord(addr, data, debug_memory);
        } else { // word (left/right)
            uint8_t memOffset = addr & 0x3u;
            if (i->memLeft) { // store left up to next word boundary
                uint8_t bytesToWrite = 4 - memOffset;
                data = data >> (8*memOffset);
                writeByte(addr + (bytesToWrite-1), data, debug_memory);
                --bytesToWrite;
                
                while (bytesToWrite > 0) {
                    data = data >> 8;
                    writeByte(addr + (bytesToWrite-1), data, debug_memory);
                    --bytesToWrite;
                }
            } else { // store right up to previous word boundary
                uint8_t bytesToWrite = memOffset + 1;
                uint32_t _addr = addr;
                writeByte(_addr, data, debug_memory);
                --bytesToWrite;
                
                while (bytesToWrite > 0) {
                    data = data >> 8;
                    --_addr;
                    writeByte(_addr, data, debug_memory);
                    --bytesToWrite;
                }
            }
        }
    }
    
    if (i->memRead) {
        uint32_t addr = ALUresult;
        
        if (i->memType == 0b00) { // byte
            uint8_t data = readByte(addr, debug_memory);
            memResult = i->memSigned ? (*(int8_t*)&data) : data;
        } else if (i->memType == 0b01) { // halfword
            uint16_t data;
            data = readByte(addr+0, debug_memory);
            data <<= 8;
            data |= readByte(addr+1, debug_memory);
            memResult = i->memSigned ? (*(int16_t*)&data) : data;
        } else if (i->memType == 0b10) { // word
            uint32_t data = readWord(addr, debug_memory);
            memResult = data;
        } else { // word (left/right)
            uint8_t memOffset = addr & 0x3u;
            if (i->memLeft) {   // load into left, up to next word boundary
                uint32_t regVal = *(uint32_t *) &RegFile[rt];
                uint8_t bytesToRead = 4 - memOffset;
                uint32_t x = ~0u;
                uint32_t data = readByte(addr, debug_memory);
                --bytesToRead;
                x = x >> 8;
                
                while (bytesToRead > 0) {
                    addr++;
                    data = (data << 8) | readByte(addr, debug_memory);
                    --bytesToRead;
                    x = x >> 8;
                }
                data = data << (memOffset * 8);
                memResult = data | (x & regVal);
            } else { // load into right, from previous word boundary up
                uint32_t regVal = *(uint32_t *) &RegFile[rt];
                uint8_t bytesToRead = 1 + memOffset;
                uint32_t x = ~0u;
                uint32_t baseAddr = addr - memOffset;
                uint32_t data = readByte(baseAddr, debug_memory);
                --bytesToRead;
                x = x << 8;
                
                while (bytesToRead > 0) {
                    baseAddr++;
                    data = (data << 8) | readByte(baseAddr, debug_memory);
                    --bytesToRead;
                    x = x << 8;
                }
                memResult = data | (x & regVal);
            }
        }
    }
    
    // branch
    if (i->type == BTYPE) {
        delayInfo.delay_slot = PC + 4;
        delayInfo.target_offset = (((int32_t)((int16_t)imm)) << 2);
        delayInfo.condition = i->invertZ ? !zero : zero;
        delayInfo.nullify = i->retire;
        delayInfo.relative = true;
        
        if (opcode == REGIMM)
        switch (rt) {
        case BGEZAL:
        case BLTZAL:
            RegFile[31] = PC + 8;   // link
            break;
        default: break;
        }
        
        inDelaySlot = true;
    }
    
    // jump
    if (i->type == JTYPE) {
        uint32_t temp;
        
        delayInfo.delay_slot = PC + 4;
        delayInfo.target_offset = 0;
        delayInfo.condition = true;
        delayInfo.nullify = i->retire;
        delayInfo.relative = false;

        switch (opcode) {
        case JAL:
            RegFile[31] = PC + 8;   // return address
        case J:
            delayInfo.newPC = (newPC & 0xF0000000u) | (instr_index << 2);
            break;
        case SPECIAL:
            break;
        default:
            fprintf(stderr, "unknown opcode %x\n", opcode);
            break;
        }
        
        if (opcode == SPECIAL)
        switch (func) {
        case JALR:
            temp = RegFile[rs];
            RegFile[rd ? rd : 31] = PC + 8;
            delayInfo.newPC = temp;
            break;
        case JR:
            delayInfo.newPC = RegFile[rs];
            break;
        default:
            fprintf(stderr, "unknown func %x\n", func);
            break;
        }
        
        inDelaySlot = true;
    }
}

void WritebackStage(uint32_t PC, uint32_t *newPCptr) {
    if (i->regWrite) {
        int32_t data = i->memToReg ? memResult : ALUresult;
        
        if (i->type == RTYPE) // dest = rd
            RegFile[rd] = data;
        else if (i->type == ITYPE) // dest = rt
            RegFile[rt] = data;
    }
    
    RegFile[0] = 0;
    
    // done
    
    // syscall?
    if (i->type == RTYPE && func == SYSCALL)
        SyscallExe(RegFile[2]);
    
    // jump or branch now?
    if (inDelaySlot && delayInfo.delay_slot == PC) {
        if (delayInfo.condition) {
            if (delayInfo.relative)
                *newPCptr = PC + delayInfo.target_offset;
            else
                *newPCptr = delayInfo.newPC;
            printf("jumping to %08x\n", *newPCptr);
        }
        
        memset(&delayInfo, 0, sizeof(delayInfo));
        inDelaySlot = false;
    }
}

void PrintInstruction(void) {
    printf("%08x:  %s ", pc, i->name);
    
    switch (i->type) {
    case RTYPE: // R
        //printf("rd, rs, rt");
        if (i->aluSrc == ALUSRC_SHAMT)
            printf("$%u, $%u, %u", rd, rt, shamt);
        else if (i->regWrite) {
            if (i->aluSrc != ALUSRC_RT)
                printf("$%u", rd);
            else
                printf("$%u, $%u, $%u", rd, rs, rt);
        } else if (i->aluOp == ALUOP_MTHI || i->aluOp == ALUOP_MTLO)
            printf("$%u", rs);
          else if (rs || rt) // div/etc
            printf("$%u, $%u", rs, rt);
        break;
    case ITYPE: // I
        //printf("rt, rs, imm");
        if (i->aluOp == ALUOP_LUI)
            printf("$%u, %u", rt, imm);
        else if (i->aluOp == ALUOP_ADD && i->aluSrc == ALUSRC_IMM && (i->memRead || i->memWrite))
            printf("$%u, %d($%u)", rt, (int16_t)imm, rs);
        else
            printf("$%u, $%u, %d", rt, rs, imm);
        break;
    case BTYPE:
        if (i->aluSrc == ALUSRC_RT)
            printf("$%u, $%u, %x", rs, rt, ((int32_t)(*(int16_t*)&imm)) << 2);
        else
            printf("$%u, %x", rs, imm);
        break;
    case JTYPE:
        if (i->aluSrc == ALUSRC_INSTR_INDEX)
            printf("%x", instr_index);
        else if (rd == 0)
            printf("$%u", rs);
        else
            printf("$%u, $%u", rd, rs);
        break;
    default: break;
    }
    
    if (inDelaySlot)
        printf("\t(branch delay slot)");
    printf("\n");
}
