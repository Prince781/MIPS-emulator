#ifndef INSTRUCTION_H
#define INSTRUCTION_H

typedef enum _ALUSource {
    ALUSRC_RT           = 0b0000,
    ALUSRC_IMM          = 0b0001,
    ALUSRC_SHAMT        = 0b0010,
    ALUSRC_INSTR_INDEX  = 0b0011,
    ALUSRC_HI           = 0b0100,
    ALUSRC_LO           = 0b0101,
    ALUSRC_RS           = 0b0110,
    ALUSRC_IMMU         = 0b0111,
    ALUSRC_REGIMM       = 0b1000
} ALUSource;

typedef enum _InstrType {
    RTYPE = 0b00,
    ITYPE = 0b01,
    BTYPE = 0b10,
    JTYPE = 0b11
} InstrType;

// defines an instruction
typedef struct _instr {
    const char name[10];
    
    union {
        struct __attribute__((packed)) {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
            const ALUSource aluSrc  : 4;
            const InstrType type    : 2;
            const bool regWrite     : 1;
            const bool memWrite     : 1;
            const bool memRead      : 1;
            const bool memToReg     : 1;
            const uint8_t memType   : 2;
            const bool memSigned    : 1;
            const bool memLeft      : 1;
            const bool retire       : 1;
            const bool invertZ      : 1;
#else
            const bool invertZ      : 1;
            const bool retire       : 1;
            const bool memLeft      : 1;
            const bool memSigned    : 1;
            const uint8_t memType   : 2;
            const bool memToReg     : 1;
            const bool memRead      : 1;
            const bool memWrite     : 1;
            const bool regWrite     : 1;
            const InstrType type    : 2;
            const ALUSource aluSrc  : 4;
#endif
        };
        // [aluSrc:4][type:2][regWrite][memWrite][memRead][memToReg]
        // aluSrc (00 = rt, 01 = imm, 10 = shamt, 11 = instr_index)
        // type (00 = R-format, 01 = I-format, 10 = branch, 11 = J-format)
        // regWrite?
        // memWrite?
        // memRead?
        // memToReg?
        // [memType:2][memSigned][memLeft][retire][unused]
        // memType (00 = byte, 01 = halfword, 10 = word, 11 = word (special case: left/right))
        // memSigned (0 = unsigned, 1 = signed)
        // memLeft (0 = load right, 1 = load left; only applies when memType==11)
        // retire (1 = retire next instruction if branch condition fails)
        // invertZ (1 = use inverted value of (ALUresult==0))
        const uint16_t control;
    };
    
    // 0 = nop, 1 = add, etc
    const uint8_t aluOp;
} instr;

extern const instr Iinstructions[];
extern const instr Rinstructions[];
extern const instr Binstructions[];

struct delay_info {
    uint32_t delay_slot;    // PC of the delayed instruction
    int32_t target_offset;  // offset to PC (branch)
    uint32_t newPC;         // set new PC instead (jump)
    bool condition;
    bool nullify;     // nullify instruction if condition was not satisfied?
    bool relative;
};

extern uint32_t CurrentInstruction;

extern bool inDelaySlot;
extern struct delay_info delayInfo;

void InstructionFetch(uint32_t PC);

void InstructionDecode(void);

void InstructionExecute(void);

void MemoryStage(uint32_t PC, uint32_t newPC);

void WritebackStage(uint32_t PC, uint32_t *newPCptr);

void PrintInstruction(void);

#endif
