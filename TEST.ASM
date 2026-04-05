LDA #0
STA 0      ; fib n-2
LDA #1
STA 1      ; fib n-1
LDX #10
           ; loop
    LDA 0
    CLC
    ADC 1
    STA 2  ; fib n
    LDA 1
    STA 0
    LDA 2
    STA 1
    DEX
    BNE #6 ; jump to loop
