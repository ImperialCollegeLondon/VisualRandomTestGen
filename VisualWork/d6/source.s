MOV R0, #0x7FFFFFFF
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0x1
MOV R1, #0x80000000
MOV R2, #0x7ffffffd
MOV R3, #0xfffffffe
MOV R4, #0x80000001
MOV R5, #0xfffffffe
MOV R6, #0x4
MOV R7, #0x7ffffffd
MOV R8, #0x7ffffffd
MOV R9, #0x0
MOV R10, #0x1
MOV R11, #0x7ffffffe
MOV R12, #0x3
MOV R13, #0x80000003
MOV R14, #0x7ffffffc


MOV R8 ,R0	, ROR #3
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
