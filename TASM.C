#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TASM_CheckOpr(s1, s2) ((s1)[0]==(s2)[0] && (s1)[1]==(s2)[1] && (s1)[2]==(s2)[2])
#define TASM_OverflowMem(v) (v) = (v) & 0xFF
#define TASM_SetZN(m, v) do {          \
    (v) &= 0xFF;                       \
    if ((v) == 0) { (m)->Z = 1; }      \
    else { (m)->Z = 0; }               \
    if ((v) & 0x80) { (m)->N = 1; }    \
    else { (m)->N = 0; } } while(0)
#define TASM_SetC(m, v1, v2) do {      \
    if((v1) >= (v2)) { (m)->C = 1;}    \
    else { (m)->C = 0; } } while(0)    \

enum TASM_INSTRUCTIONS {
    NOP,
    LDA, LDX, LDY, STA, STX, STY, TAX, TAY, TSX, TXA, TYA, TXS, /* memory */
    PHA, PLA, CLC, CLV,                                         /* stack and flags */
    DEC, DEX, DEY, INC, INX, INY, ADC, SBC, ROL, ROR,           /* arithmetic */
    CMP, CPX, CPY, BEQ, BNE, BMI, BPL, JMP, JSR, RTS            /* condition and skip*/
};

enum TASM_ADDRESSING {
    NOA, /* no addressing */
    IMP, /* no operand */
    IMM, /* literal value */
    ABS  /* value address */
};

struct TASM_Machine {
    int** ROM; int PC;           /* array that represents program (opcode & memory) */
    int* RAM; int SP;            /* array that represents memory */ 
    int A; int X; int Y;         /* registers: accumulator, index x, index y */
    int C; int Z; int V; int N;  /* flags: carry, zero, overflow, negative */
};

/* parses string into token */
void* TASM_Parser(char* S, struct TASM_Machine* M) {
    int* T = malloc(sizeof(int)*3);
    int Idx = 0;
    int SubIdx = 0;
    int IsHex = 0;
    int IsBin = 0;
    char* Line = NULL;
    
    T[0] = 0;
    T[1] = 0;
    T[2] = 0;
    
    for(Idx = 0; S[Idx] != '\0'; Idx++) {
        if(S[Idx] == ' ' || S[Idx] == '\t' || S[Idx] == '\n') {
            continue;
        }
        if(S[Idx] == ';') { break; }
        Line = realloc(Line, sizeof(char)*(SubIdx+2));
        Line[SubIdx] = S[Idx];
        Line[SubIdx+1] = '\0';
        SubIdx++;
    }
    if(SubIdx < 3) { free(Line); return T; }
    
    if(TASM_CheckOpr(Line, "NOP")) { T[0] = NOP; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "LDA")) { T[0] = LDA; }
    else if(TASM_CheckOpr(Line, "LDX")) { T[0] = LDX; }
    else if(TASM_CheckOpr(Line, "LDY")) { T[0] = LDY; }
    else if(TASM_CheckOpr(Line, "STA")) { T[0] = STA; }
    else if(TASM_CheckOpr(Line, "STX")) { T[0] = STX; }
    else if(TASM_CheckOpr(Line, "STY")) { T[0] = STY; }
    else if(TASM_CheckOpr(Line, "TAX")) { T[0] = TAX; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "TAY")) { T[0] = TAY; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "TSX")) { T[0] = TSX; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "TXA")) { T[0] = TXA; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "TYA")) { T[0] = TYA; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "TXS")) { T[0] = TXS; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "PHA")) { T[0] = PHA; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "PLA")) { T[0] = PLA; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "CLC")) { T[0] = CLC; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "CLV")) { T[0] = CLV; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "DEC")) { T[0] = DEC; }
    else if(TASM_CheckOpr(Line, "DEX")) { T[0] = DEX; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "DEY")) { T[0] = DEY; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "INC")) { T[0] = INC; }
    else if(TASM_CheckOpr(Line, "INX")) { T[0] = INX; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "INY")) { T[0] = INY; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "ADC")) { T[0] = ADC; }
    else if(TASM_CheckOpr(Line, "SBC")) { T[0] = SBC; }
    else if(TASM_CheckOpr(Line, "ROL")) { T[0] = ROL; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "ROR")) { T[0] = ROR; T[2] = IMP; }
    else if(TASM_CheckOpr(Line, "CMP")) { T[0] = CMP; }
    else if(TASM_CheckOpr(Line, "CPX")) { T[0] = CPX; }
    else if(TASM_CheckOpr(Line, "CPY")) { T[0] = CPY; }
    else if(TASM_CheckOpr(Line, "BEQ")) { T[0] = BEQ; }
    else if(TASM_CheckOpr(Line, "BNE")) { T[0] = BNE; }
    else if(TASM_CheckOpr(Line, "BMI")) { T[0] = BMI; }
    else if(TASM_CheckOpr(Line, "BPL")) { T[0] = BPL; }
    else if(TASM_CheckOpr(Line, "JMP")) { T[0] = JMP; }
    else if(TASM_CheckOpr(Line, "JSR")) { T[0] = JSR; }
    else if(TASM_CheckOpr(Line, "RTS")) { T[0] = RTS; T[2] = IMP; }
    
    if(T[2] == IMP) { free(Line); return T; }
    
    Idx = 3;
    if(Line[Idx] == '#') { T[2] = IMM; Idx++; }
    else { T[2] = ABS; }
    if(Line[Idx] == '$') { IsHex = 1; Idx++; }
    else if(Line[Idx] == '%') { IsBin = 1; Idx++; }
    
    while(Line[Idx] != '\0') {
        if(IsHex) {
            T[1] *= 16;
            if(Line[Idx] >= '0' && Line[Idx] <= '9') {
                T[1] += Line[Idx]-'0';
            }
            else if(Line[Idx] >= 'A' && Line[Idx] <= 'F') {
                T[1] += Line[Idx]-'A'+10;
            }
        }
        else if(IsBin && (Line[Idx] == '0' || Line[Idx] == '1')) {
            T[1] *= 2;
            T[1] += Line[Idx]-'0';
        }
        else if(Line[Idx] >= '0' && Line[Idx] <= '9') {
            T[1] *= 10;
            T[1] += Line[Idx]-'0';
        }
        Idx++;
    }
    if(T[2] == IMM) { TASM_OverflowMem(T[1]); }
    
    free(Line);
    return T;
}

/* evaluates current token */
void TASM_Eval(struct TASM_Machine* M) {
    int Value = 0;
    int Carry = 0;
    int* Data = &Value;
    if(M->PC < 0 || M->PC > 0xFFFF) { return; }
    if(M->ROM[M->PC][2] == IMM) { *Data = M->ROM[M->PC][1]; }
    if(M->ROM[M->PC][2] == ABS) { Data = &M->RAM[M->ROM[M->PC][1]]; }
    if(M->C) { Carry = 1; }
    
    switch(M->ROM[M->PC][0]) {
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
        case CLC: M->C = 0; break;
        case CLV: M->V = 0; break;
        case DEC: (*Data)--; TASM_SetZN(M, *Data); break;
        case DEX: M->X--; TASM_SetZN(M, M->X); break;
        case DEY: M->Y--; TASM_SetZN(M, M->Y); break;
        case INC: (*Data)++; TASM_SetZN(M, *Data); break;
        case INX: M->X++; TASM_SetZN(M, M->X); break;
        case INY: M->Y++; TASM_SetZN(M, M->Y); break;
        case ADC: {
            int Result = M->A + *Data + Carry;
            if (~(M->A ^ Result) & (M->A ^ *Data) & 0x80) { M->V = 1; }
            else { M->V = 0; }
            M->A = Result;
            TASM_SetZN(M, M->A);
            TASM_SetC(M, M->A, 0xFF);
            break;
        }
        case SBC: {
            int Inverted = (*Data) ^ 0xFF;
            int Result = M->A + Inverted + Carry;
            if ((M->A ^ Result) & (M->A ^ Inverted) & 0x80) { M->V = 1; }
            else { M->V = 0; }
            M->A = Result;
            TASM_SetZN(M, M->A);
            TASM_SetC(M, M->A, 0xFF);
            break;
        }
        case ROL: {
            int Old = M->C;
            if(M->A & 0x80) { M->C = 1; }
            else { M->C = 0; }
            M->A = (M->A << 1) | Old;
            TASM_SetZN(M, M->A);
            break;
        }
        case ROR: {
            int Old = M->C;
            if(M->A & 0x01) { M->C = 1; }
            else { M->C = 0; }
            M->A = (M->A >> 1) | (Old << 7);
            TASM_SetZN(M, M->A);
            break;
        }
        case CMP: {
            int Result = (M->A - *Data) & 0xFF;
            TASM_SetZN(M, Result);
            TASM_SetC(M, M->A, *Data);
            break;
        }
        case CPX: {
            int Result = (M->X - *Data) & 0xFF;
            TASM_SetZN(M, Result);
            TASM_SetC(M, M->X, *Data);
            break;
        }
        case CPY: {
            int Result = (M->Y - *Data) & 0xFF;
            TASM_SetZN(M, Result);
            TASM_SetC(M, M->Y, *Data);
            break;
        }
        case BEQ: if(M->Z) {M->PC = *Data-1;} break;
        case BNE: if(!M->Z) {M->PC = *Data-1;} break;
        case BMI: if(M->N) {M->PC = *Data-1;} break;
        case BPL: if(!M->N) {M->PC = *Data-1;} break;
        case JMP: M->PC = *Data-1; break;
        case JSR:
            M->RAM[0x100 + M->SP] = (M->PC >> 8) & 0xFF;
            M->SP--;
            M->RAM[0x100 + M->SP] = M->PC & 0xFF;
            M->SP--;
            M->PC = *Data-1;
            break;
        case RTS:
            M->SP++;
            M->PC = M->RAM[0x100 + M->SP];
            M->SP++;
            M->PC = (M->PC << 8) | M->RAM[0x100 + M->SP];
            break;
    }
    
    TASM_OverflowMem(M->X);
    TASM_OverflowMem(M->Y);
    TASM_OverflowMem(M->A);
    TASM_OverflowMem(M->SP);
    if(M->ROM[M->PC][2] == IMM) { TASM_OverflowMem(*Data); }
    M->PC++;
}

/* creates a new machine */
void* TASM_Start(char* F) {
    struct TASM_Machine* M = malloc(sizeof(struct TASM_Machine));
    
    FILE* Program = fopen(F, "r");
    char* Instruction = malloc(sizeof(char)*21);
    
    int Idx = 0;
    if(!Program) { printf("--<File missing!>--\n"); exit(1); }
    
    M->ROM = calloc(1, sizeof(int*));
    M->RAM = calloc(0xFFFF, sizeof(int));
    M->PC = 0;
    M->SP = 0xFF;
    M->A = 0;
    M->X = 0;
    M->Y = 0;
    M->C = 0;
    M->Z = 0;
    M->V = 0;
    M->N = 0;
    
    while(fgets(Instruction, 21, Program)) {
        int* T = TASM_Parser(Instruction, M);
        M->ROM = realloc(M->ROM, sizeof(int*)*(Idx+2));
        M->ROM[Idx] = T;
        M->ROM[Idx+1] = NULL;
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
    
	while(M->ROM[M->PC+1] != NULL) {
		TASM_Eval(M);
        
		printf("\033[2J\033[H");
		printf("--<CPU>----------------------\n");
        printf("\"%s\"\n", Name);
        printf("(PC)$%X,  (SP)$%X\n", M->PC, M->SP);
        printf("(A)$%X,  (X)$%X,  (Y)$%X\n", M->A, M->X, M->Y);
        printf("(C)%d,  (Z)%d,  (V)%d,  (N)%d\n", M->C, M->Z, M->V, M->N);
        
		printf("--<RAM>----------------------\n");
		for(Idx = 0; Idx < 0xFFFF; Idx++) {
			if(M->RAM[Idx] != 0) { printf("($%X)$%X ", Idx, M->RAM[Idx]); }
		}
        
        Start = clock();
        while(clock() < Start + (CLOCKS_PER_SEC * 0.1)) {}
	}
}

int main(int ArgC, char* ArgV[]) {
    struct TASM_Machine* M;
    int Idx;
    if(!ArgV[1]) { printf("--<File missing!>--\n"); exit(1); }
    
    M = TASM_Start(ArgV[1]);
    TASM_Execute(M, ArgV[1]);
    
    for(Idx = 0; M->ROM[Idx+1] != NULL; Idx++) {
    	free(M->ROM[Idx]);
    }
    free(M->RAM);
    return 0;
}