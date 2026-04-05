#include "tasm.h"

int main(int ArgC, char* ArgV[]) {
    struct TASM_Machine* M = TASM_Open(ArgV[1]);
    TASM_Debug(M);
    TASM_Close(M);
}
