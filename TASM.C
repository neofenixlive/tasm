#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define C 0x1
#define Z 0x2
#define N 0x4

#define TASM_CheckOpr(s1, s2) ((s1)[0]==(s2)[0] && (s1)[1]==(s2)[1] && (s1)[2]==(s2)[2])
#define TASM_SetZN(m, v) do {        \
    if ((v) == 0) { (m)->P |= Z; }   \
    else { (m)->P &= ~Z; }           \
    if ((v) & 0x80) { (m)->P |= N; } \
    else { (m)->P &= ~N; } } while(0)
#define TASM_SetC(m, v1, v2) do {    \
    if((v1) >= (v2)) { (m)->P |= C;} \
    else { (m)->P &= ~C; } } while(0)

enum TASM_INSTRUCTIONS {
    NOP,
    LDA, LDX, LDY, STA, STX, STY, TAX, TAY, TSX, TXA, TYA, TXS, /* memory */
    PHP, PLP, PHA, PLA, CLC,                                    /* stack and flags */
    DEC, DEX, DEY, INC, INX, INY, ADC, SBC, ASL, LSR, ROL, ROR, /* arithmetic */
    CMP, CPX, CPY, BEQ, BNE, BMI, BPL, JMP, JSR, RTS            /* condition and skip*/
};

enum TASM_ADDRESSING {
    NOA, /* no addressing */
    IMP, /* no operand */
    IMM, /* literal value */
    ABS  /* value address */
};

struct TASM_Machine {
    unsigned int** ROM; unsigned int PC;               /* array that represents program */
    unsigned char* RAM; unsigned char SP;              /* array that represents memory */
    unsigned char A; unsigned char X; unsigned char Y; /* registers */
    unsigned char P;                                   /* flags */
};

/* parses string into token */
void* TASM_Parser(char* S, struct TASM_Machine* M) {
    int* T = malloc(sizeof(int) * 3);
    int Idx = 0;
    int SubIdx = 0;
    int IsHex = 0;
    int IsBin = 0;
    int SaveAs16 = 0;
    char* Line = NULL;
    void* Temp = NULL;

    T[0] = 0;
    T[1] = 0;
    T[2] = 0;

    for (Idx = 0; S[Idx] != '\0'; Idx++) {
        if (S[Idx] == ' ' || S[Idx] == '\t' || S[Idx] == '\n') {
            continue;
        }
        if (S[Idx] == ';') { break; }

        Temp = realloc(Line, sizeof(char) * (SubIdx + 2));
        if (Temp == NULL) {
            free(Line);
            free(T);
            printf("--<Instruction parser failed!>--\n");
            exit(1);
        }
        Line = Temp;

        Line[SubIdx] = S[Idx];
        Line[SubIdx + 1] = '\0';
        SubIdx++;
    }
    if (SubIdx < 3) { free(Line); return T; }

    if (TASM_CheckOpr(Line, "NOP")) { T[0] = NOP; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "LDA")) { T[0] = LDA; }
    else if (TASM_CheckOpr(Line, "LDX")) { T[0] = LDX; }
    else if (TASM_CheckOpr(Line, "LDY")) { T[0] = LDY; }
    else if (TASM_CheckOpr(Line, "STA")) { T[0] = STA; }
    else if (TASM_CheckOpr(Line, "STX")) { T[0] = STX; }
    else if (TASM_CheckOpr(Line, "STY")) { T[0] = STY; }
    else if (TASM_CheckOpr(Line, "TAX")) { T[0] = TAX; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "TAY")) { T[0] = TAY; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "TSX")) { T[0] = TSX; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "TXA")) { T[0] = TXA; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "TYA")) { T[0] = TYA; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "TXS")) { T[0] = TXS; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "PHP")) { T[0] = PHP; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "PLP")) { T[0] = PLP; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "PHA")) { T[0] = PHA; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "PLA")) { T[0] = PLA; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "CLC")) { T[0] = CLC; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "DEC")) { T[0] = DEC; }
    else if (TASM_CheckOpr(Line, "DEX")) { T[0] = DEX; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "DEY")) { T[0] = DEY; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "INC")) { T[0] = INC; }
    else if (TASM_CheckOpr(Line, "INX")) { T[0] = INX; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "INY")) { T[0] = INY; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "ADC")) { T[0] = ADC; }
    else if (TASM_CheckOpr(Line, "SBC")) { T[0] = SBC; }
    else if (TASM_CheckOpr(Line, "ASL")) { T[0] = ASL; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "LSR")) { T[0] = LSR; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "ROL")) { T[0] = ROL; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "ROR")) { T[0] = ROR; T[2] = IMP; }
    else if (TASM_CheckOpr(Line, "CMP")) { T[0] = CMP; }
    else if (TASM_CheckOpr(Line, "CPX")) { T[0] = CPX; }
    else if (TASM_CheckOpr(Line, "CPY")) { T[0] = CPY; }
    else if (TASM_CheckOpr(Line, "BEQ")) { T[0] = BEQ; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "BNE")) { T[0] = BNE; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "BMI")) { T[0] = BMI; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "BPL")) { T[0] = BPL; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "JMP")) { T[0] = JMP; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "JSR")) { T[0] = JSR; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "RTS")) { T[0] = RTS; T[2] = IMP; }

    if (T[2] == IMP) { free(Line); return T; }

    Idx = 3;
    if (Line[Idx] == '#') { T[2] = IMM; Idx++; }
    else { T[2] = ABS; }
    if (Line[Idx] == '$') { IsHex = 1; Idx++; }
    else if (Line[Idx] == '%') { IsBin = 1; Idx++; }

    while (Line[Idx] != '\0') {
        if (IsHex) {
            T[1] *= 16;
            if (Line[Idx] >= '0' && Line[Idx] <= '9') {
                T[1] += Line[Idx] - '0';
            }
            else if (Line[Idx] >= 'A' && Line[Idx] <= 'F') {
                T[1] += Line[Idx] - 'A' + 10;
            }
        }
        else if (IsBin && (Line[Idx] == '0' || Line[Idx] == '1')) {
            T[1] *= 2;
            T[1] += Line[Idx] - '0';
        }
        else if (Line[Idx] >= '0' && Line[Idx] <= '9') {
            T[1] *= 10;
            T[1] += Line[Idx] - '0';
        }
        Idx++;
    }
    if (!SaveAs16 && T[2] != ABS) { T[1] &= 0xFF; }

    free(Line);
    return T;
}

/* evaluates current token */
void TASM_Eval(struct TASM_Machine* M) {
    int Value = 0;
    int Carry = 0;
    int* Data = &Value;
    if (M->ROM[M->PC][2] == IMM) { *Data = M->ROM[M->PC][1]; }
    if (M->ROM[M->PC][2] == ABS) { Data = &M->RAM[M->ROM[M->PC][1]]; }
    if (M->P & C) { Carry = 1; }

    switch (M->ROM[M->PC][0]) {
    default: break;
    case LDA: M->A = *Data; break;
    case LDX: M->X = *Data; break;
    case LDY: M->Y = *Data; break;
    case STA: *Data = M->A; break;
    case STX: *Data = M->X; break;
    case STY: *Data = M->Y; break;
    case TAX: M->X = M->A; break;
    case TAY: M->Y = M->A; break;
    case TSX: M->X = M->SP; break;
    case TXA: M->A = M->X; break;
    case TYA: M->A = M->Y; break;
    case TXS: M->SP = M->X; break;
    case PHA: M->RAM[0x100 + M->SP] = M->A; M->SP--; break;
    case PLA: M->SP++; M->A = M->RAM[0x100 + M->SP]; M->RAM[0x100 + M->SP] = 0; break;
    case PHP: M->RAM[0x100 + M->SP] = M->P; M->SP--; break;
    case PLP: M->SP++; M->P = M->RAM[0x100 + M->SP]; M->RAM[0x100 + M->SP] = 0; break;
    case CLC: M->P &= ~C; break;
    case DEC: (*Data)--; TASM_SetZN(M, *Data); break;
    case DEX: M->X--; TASM_SetZN(M, M->X); break;
    case DEY: M->Y--; TASM_SetZN(M, M->Y); break;
    case INC: (*Data)++; TASM_SetZN(M, *Data); break;
    case INX: M->X++; TASM_SetZN(M, M->X); break;
    case INY: M->Y++; TASM_SetZN(M, M->Y); break;
    case ADC: {
        int Result = M->A + *Data + Carry;
        M->A = Result;
        TASM_SetZN(M, M->A);
        TASM_SetC(M, Result, 0x100);
        break;
    }
    case SBC: {
        int Inverted = (*Data) ^ 0xFF;
        int Result = M->A + Inverted + Carry;
        M->A = Result;
        TASM_SetZN(M, M->A);
        TASM_SetC(M, Result, 0x100);
        break;
    }
    case ASL: {
        if (M->A & 0x80) { M->P |= C; }
        else { M->P &= ~C; }
        M->A = (M->A << 1);
        TASM_SetZN(M, M->A);
        break;
    }
    case LSR: {
        if (M->A & 0x1) { M->P |= C; }
        else { M->P &= ~C; }
        M->A = (M->A >> 1);
        TASM_SetZN(M, M->A);
        break;
    }
    case ROL: {
        if (M->A & 0x80) { M->P |= C; }
        else { M->P &= ~C; }
        M->A = (M->A << 1) | Carry;
        TASM_SetZN(M, M->A);
        break;
    }
    case ROR: {
        if (M->A & 0x1) { M->P |= C; }
        else { M->P &= ~C; }
        M->A = (M->A >> 1) | (Carry << 7);
        TASM_SetZN(M, M->A);
        break;
    }
    case CMP: {
        unsigned char Result = (M->A - *Data);
        TASM_SetZN(M, Result);
        TASM_SetC(M, M->A, *Data);
        break;
    }
    case CPX: {
        unsigned char Result = (M->X - *Data);
        TASM_SetZN(M, Result);
        TASM_SetC(M, M->X, *Data);
        break;
    }
    case CPY: {
        unsigned char Result = (M->Y - *Data);
        TASM_SetZN(M, Result);
        TASM_SetC(M, M->Y, *Data);
        break;
    }
    case BEQ: if (M->P & Z) { M->PC = *Data - 2; } break;
    case BNE: if (!(M->P & Z)) { M->PC = *Data - 2; } break;
    case BMI: if (M->P & N) { M->PC = *Data - 2; } break;
    case BPL: if (!(M->P & N)) { M->PC = *Data - 2; } break;
    case JMP: M->PC = *Data - 2; break;
    case JSR:
        M->RAM[0x100 + M->SP] = (M->PC >> 8) & 0xFF;
        M->SP--;
        M->RAM[0x100 + M->SP] = M->PC & 0xFF;
        M->SP--;
        M->PC = *Data - 2;
        break;
    case RTS:
        M->SP++;
        M->PC = M->RAM[0x100 + M->SP];
        M->SP++;
        M->PC = (M->PC << 8) | M->RAM[0x100 + M->SP];
        break;
    }
    M->PC++;
}

/* creates a new machine */
void* TASM_Start(char* F) {
    struct TASM_Machine* M = malloc(sizeof(struct TASM_Machine));

    FILE* Program = fopen(F, "r");
    char* Instruction = malloc(sizeof(char) * 21);

    int Idx = 0;
    void* Temp = NULL;
    if (!Program) { printf("--<File missing!>--\n"); exit(1); }

    M->ROM = calloc(1, sizeof(int*));
    M->RAM = calloc(0x10000, sizeof(char));
    M->PC = 0x0000;
    M->SP = 0xFF;
    M->A = 0;
    M->X = 0;
    M->Y = 0;
    M->P = 0;

    while (fgets(Instruction, 21, Program)) {
        int* T = TASM_Parser(Instruction, M);

        Temp = realloc(M->ROM, sizeof(int*) * (Idx + 2));
        if (Temp == NULL) {
            free(T);
            free(Instruction);
            fclose(Program);
            free(M->ROM);
            free(M->RAM);
            free(M);
            printf("--<Read-only memory failed!>--\n");
            exit(1);
        }
        M->ROM = Temp;

        M->ROM[Idx] = T;
        M->ROM[Idx + 1] = NULL;
        Idx++;
    }

    free(Instruction);
    fclose(Program);
    return M;
}

/* runs a machine */
void TASM_Execute(struct TASM_Machine* M, char* Name) {
    int Idx;
    int SubIdx;
    clock_t Start;

    while (M->ROM[M->PC] != NULL) {
        TASM_Eval(M);

        printf("\033[2J\033[H");
        printf("--<CPU>----------------------\n");
        printf("\"%s\"\n", Name);
        printf("(PC)$%X,  (SP)$%X\n", M->PC, M->SP);
        printf("(A)$%X,  (X)$%X,  (Y)$%X\n", M->A, M->X, M->Y);
        printf("(C)%d (V)%d (N)%d (Z)%d\n", M->P & C != 0, M->P & V != 0, M->P & N != 0, M->P & Z != 0);

        printf("--<RAM>----------------------\n");
        for (Idx = 0; Idx < 0xFFFF; Idx++) {
            if (M->RAM[Idx] != 0) { printf("($%X)$%X ", Idx, M->RAM[Idx]); }
        }

        Start = clock();
        while (clock() < Start + (CLOCKS_PER_SEC * 0.1)) {}
    }
}

int main(int ArgC, char* ArgV[]) {
    struct TASM_Machine* M;
    if (!ArgV[1]) { printf("--<File missing!>--\n"); exit(1); }

    M = TASM_Start(ArgV[1]);
    TASM_Execute(M, ArgV[1]);

    free(M);
    return 0;
}