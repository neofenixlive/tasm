#ifndef __TINYASSEMBLY
#define __TINYASSEMBLY

#include <stdio.h>
#include <stdlib.h>

#define FLAG_C 0x1
#define FLAG_Z 0x2
#define FLAG_N 0x4

#define TASM_CheckOpr(s1, s2) ((s1)[0]==(s2)[0] && (s1)[1]==(s2)[1] && (s1)[2]==(s2)[2])
#define TASM_SetZN(m, v) do {             \
    if ((v) == 0) { (m)->P |= FLAG_Z; }   \
    else { (m)->P &= ~FLAG_Z; }           \
    if ((v) & 0x80) { (m)->P |= FLAG_N; } \
    else { (m)->P &= ~FLAG_N; } } while(0)
#define TASM_SetC(m, v1, v2) do {         \
    if((v1) >= (v2)) { (m)->P |= FLAG_C;} \
    else { (m)->P &= ~FLAG_C; } } while(0)

enum TASM_INSTRUCTIONS {
    NOP,
    LDA, LDX, LDY, STA, STX, STY, TAX, TAY, TXA, TYA, /* memory */
    PHA, PLA, CLC, SEC,                               /* stack and flags */
    DEC, DEX, DEY, INC, INX, INY, ADC, SBC, ROL, ROR, /* arithmetic */
    CMP, CPX, CPY, BEQ, BNE, BMI, BPL, JMP, JSR, RTS, /* control flow */
    END
};

enum TASM_ADDRESS {
    NOA, /* no address */
    IMP, /* no value */
    IMM, /* define value */
    ABS, /* address value */
    IDX, /* index X */
    IDY  /* index Y */
};

/* assembly enviroment */
struct TASM_Machine {
    unsigned char* ROM; unsigned int PC;               /* program */
    unsigned char* RAM; unsigned char SP;              /* memory */
    unsigned char A; unsigned char X; unsigned char Y; /* registers */
    unsigned char P;                                   /* flags */
};

typedef struct TASM_Machine TASM_Machine;
void* TASM_Open(char* F);
void TASM_Close(struct TASM_Machine* M);
void TASM_Execute(struct TASM_Machine* M);
void TASM_Debug(struct TASM_Machine* M);

#endif