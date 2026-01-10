#include "tasm.h"

/* parses string into token */
void* TASM_Parser(char* S) {
    unsigned int* T = calloc(4, sizeof(int));
    int Idx = 0;
    int SubIdx = 0;
    int IsHex = 0;
    int IsBin = 0;
    int SaveAs16 = 0;
    
    char* Line = NULL;
    void* Temp = NULL;

    /* removes whitespace and ignores comments */
    for (Idx = 0; S[Idx] != '\0'; Idx++) {
        if (S[Idx] == ' ' || S[Idx] == '\t' || S[Idx] == '\n') { continue; }
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

    /* checks operation */
    if (TASM_CheckOpr(Line, "NOP")) { T[0] = NOP; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "LDA")) { T[0] = LDA; }
    else if (TASM_CheckOpr(Line, "LDX")) { T[0] = LDX; }
    else if (TASM_CheckOpr(Line, "LDY")) { T[0] = LDY; }
    else if (TASM_CheckOpr(Line, "STA")) { T[0] = STA; }
    else if (TASM_CheckOpr(Line, "STX")) { T[0] = STX; }
    else if (TASM_CheckOpr(Line, "STY")) { T[0] = STY; }
    else if (TASM_CheckOpr(Line, "TAX")) { T[0] = TAX; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "TAY")) { T[0] = TAY; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "TXA")) { T[0] = TXA; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "TYA")) { T[0] = TYA; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "PHA")) { T[0] = PHA; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "PLA")) { T[0] = PLA; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "CLC")) { T[0] = CLC; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "SEC")) { T[0] = SEC; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "DEC")) { T[0] = DEC; }
    else if (TASM_CheckOpr(Line, "DEX")) { T[0] = DEX; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "DEY")) { T[0] = DEY; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "INC")) { T[0] = INC; }
    else if (TASM_CheckOpr(Line, "INX")) { T[0] = INX; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "INY")) { T[0] = INY; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "ADC")) { T[0] = ADC; }
    else if (TASM_CheckOpr(Line, "SBC")) { T[0] = SBC; }
    else if (TASM_CheckOpr(Line, "ROL")) { T[0] = ROL; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "ROR")) { T[0] = ROR; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "CMP")) { T[0] = CMP; }
    else if (TASM_CheckOpr(Line, "CPX")) { T[0] = CPX; }
    else if (TASM_CheckOpr(Line, "CPY")) { T[0] = CPY; }
    else if (TASM_CheckOpr(Line, "BEQ")) { T[0] = BEQ; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "BNE")) { T[0] = BNE; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "BMI")) { T[0] = BMI; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "BPL")) { T[0] = BPL; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "JMP")) { T[0] = JMP; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "JSR")) { T[0] = JSR; SaveAs16 = 1; }
    else if (TASM_CheckOpr(Line, "RTS")) { T[0] = RTS; T[1] = IMP; }
    else if (TASM_CheckOpr(Line, "END")) { T[0] = END; T[1] = IMP; }

    if (T[1] == IMP) { free(Line); return T; }
    Idx = 3;
    
    /* writes data from string */
    if (Line[Idx] == '#') { T[1] = IMM; Idx++; }
    else { T[1] = ABS; }
    if (Line[Idx] == '$') { IsHex = 1; Idx++; }
    else if (Line[Idx] == '%') { IsBin = 1; Idx++; }

    while (Line[Idx] != '\0') {
        if (T[1] == ABS && Line[Idx] == ',') {
            if (Line[Idx + 1] == 'X') { T[1] = IDX; break; }
            else if (Line[Idx + 1] == 'Y') { T[1] = IDY; break; }
        }
        
        if (IsHex) {
            T[2] *= 16;
            if (Line[Idx] >= '0' && Line[Idx] <= '9') {
                T[2] += Line[Idx] - '0';
            }
            else if (Line[Idx] >= 'A' && Line[Idx] <= 'F') {
                T[2] += Line[Idx] - 'A' + 10;
            }
        }
        else if (IsBin && (Line[Idx] == '0' || Line[Idx] == '1')) {
            T[2] *= 2;
            T[2] += Line[Idx] - '0';
        }
        else if (Line[Idx] >= '0' && Line[Idx] <= '9') {
            T[2] *= 10;
            T[2] += Line[Idx] - '0';
        }
        
        Idx++;
    }
    if (SaveAs16) { T[3] = (T[2] >> 8) & 0xFF; }
    T[2] &= 0xFF;
    
    free(Line);
    return T;
}

/* evaluates current token */
void TASM_Eval(struct TASM_Machine* M) {
    unsigned int Value = (M->ROM[M->PC*4+3] << 8) | M->ROM[M->PC*4+2];
    unsigned char* Data = NULL;
    int LastC = M->P & FLAG_C;
       
    /* data for address modes */
    if (M->ROM[M->PC*4+1] == IMM) { Data = (unsigned char*)&Value; }
    else if (M->ROM[M->PC*4+1] == ABS) { Data = &M->RAM[Value]; }
    else if (M->ROM[M->PC*4+1] == IDX) { Data = &M->RAM[Value + M->X]; }
    else if (M->ROM[M->PC*4+1] == IDY) { Data = &M->RAM[Value + M->Y]; }

    /* operation execution */
    switch (M->ROM[M->PC*4]) {
    default: break;
    case LDA: M->A = *Data; break;
    case LDX: M->X = *Data; break;
    case LDY: M->Y = *Data; break;
    case STA: *Data = M->A; break;
    case STX: *Data = M->X; break;
    case STY: *Data = M->Y; break;
    case TAX: M->X = M->A; break;
    case TAY: M->Y = M->A; break;
    case TXA: M->A = M->X; break;
    case TYA: M->A = M->Y; break;
    case PHA: M->RAM[0x100 + M->SP] = M->A; M->SP--; break;
    case PLA: M->SP++; M->A = M->RAM[0x100 + M->SP]; M->RAM[0x100 + M->SP] = 0; break;
    case CLC: M->P &= ~FLAG_C; break;
    case SEC: M->P |= FLAG_C; break;
    case DEC: (*Data)--; TASM_SetZN(M, *Data); break;
    case DEX: M->X--; TASM_SetZN(M, M->X); break;
    case DEY: M->Y--; TASM_SetZN(M, M->Y); break;
    case INC: (*Data)++; TASM_SetZN(M, *Data); break;
    case INX: M->X++; TASM_SetZN(M, M->X); break;
    case INY: M->Y++; TASM_SetZN(M, M->Y); break;
    case ADC: {
        int Result = M->A + *Data + LastC;
        M->A = Result;
        TASM_SetZN(M, M->A);
        TASM_SetC(M, Result, 0x100);
        break;
    }
    case SBC: {
        int Inverted = (*Data) ^ 0xFF;
        int Result = M->A + Inverted + LastC;
        M->A = Result;
        TASM_SetZN(M, M->A);
        TASM_SetC(M, Result, 0x100);
        break;
    }
    case ROL: {
        if (M->A & 0x80) { M->P |= FLAG_C; }
        else { M->P &= ~FLAG_C; }
        M->A = (M->A << 1) | LastC;
        TASM_SetZN(M, M->A);
        break;
    }
    case ROR: {
        if (M->A & 0x1) { M->P |= FLAG_C; }
        else { M->P &= ~FLAG_C; }
        M->A = (M->A >> 1) | (LastC << 7);
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
    case BEQ: if (M->P & FLAG_Z) { M->PC = *Data - 2; } break;
    case BNE: if (!(M->P & FLAG_Z)) { M->PC = *Data - 2; } break;
    case BMI: if (M->P & FLAG_N) { M->PC = *Data - 2; } break;
    case BPL: if (!(M->P & FLAG_N)) { M->PC = *Data - 2; } break;
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
    M->PC &= 0xFFFF;
}



/* starts a machine */
void* TASM_Open(char* F) {
    struct TASM_Machine* M = malloc(sizeof(struct TASM_Machine));
    char* Instruction = malloc(sizeof(char) * 128);
    FILE* Program = fopen(F, "r");
    int Idx;
    
    if (!Program) { printf("--<File missing!>--\n"); exit(1); }

    /* setup new machine */
    M->ROM = calloc(0x4000, sizeof(char));
    M->RAM = calloc(0x2000, sizeof(char));
    
    /* write operations */
    for (Idx = 0; fgets(Instruction, 128, Program); Idx += 4) {
        unsigned int* T = TASM_Parser(Instruction);
        M->ROM[Idx] = (char)T[0];
        M->ROM[Idx+1] = (char)T[1];
        M->ROM[Idx+2] = (char)T[2];
        M->ROM[Idx+3] = (char)T[3];
        free(T);
    }
    M->ROM[Idx] = END;

    free(Instruction);
    fclose(Program);
    return M;
}

/* stops a machine */
void TASM_Close(struct TASM_Machine* M) {
    free(M->RAM);
    free(M->ROM);
    free(M);
}

/* executes a machine */
void TASM_Execute(struct TASM_Machine* M) {
    int Idx;
    for (Idx = 0; Idx < 0x2000; Idx++) { M->RAM[Idx] = 0; }
    M->PC = 0x0000;
    M->SP = 0xFF;
    M->A = 0;
    M->X = 0;
    M->Y = 0;
    M->P = 0;
    
    while (M->ROM[M->PC*4] != END) {
        TASM_Eval(M);
    }
}

/* executes a machine in debug mode */
void TASM_Debug(struct TASM_Machine* M) {
    int Idx;
    int SubIdx;
    for (Idx = 0; Idx < 0x2000; Idx++) { M->RAM[Idx] = 0; }
    M->PC = 0x0000;
    M->SP = 0xFF;
    M->A = 0;
    M->X = 0;
    M->Y = 0;
    M->P = 0;

    /* prints information and delays execution until key is pressed */
    while (M->ROM[M->PC*4] != END) {
        TASM_Eval(M);
        
        SubIdx = 0;
        printf("\033[2J");
        printf("--<TinyAssembly>--------------\n");
        printf("(PC)$%04X (SP)$%02X\n", M->PC, M->SP);
        printf("(A)$%02X (X)$%02X (Y)$%02X\n", M->A, M->X, M->Y);
        printf("(C)%d (N)%d (Z)%d\n", ((M->P & FLAG_C) != 0), ((M->P & FLAG_N) != 0), ((M->P & FLAG_Z) != 0));
        printf("--<Memory>--------------------\n");
        for (Idx = 0; Idx < 0x2000; Idx++) {
            if (M->RAM[Idx] != 0) { printf("($%04X)$%02X ", Idx, M->RAM[Idx]); SubIdx++; }
            if (SubIdx == 4) { putchar('\n'); SubIdx = 0; }
        }
        
        getchar();
    }
}