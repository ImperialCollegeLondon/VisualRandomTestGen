MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x4
MOV R1, #0x7ffffffd
MOV R2, #0x80000003
MOV R3, #0xfffffffe
MOV R4, #0xfffffffe
MOV R5, #0x80000002
MOV R6, #0x2
MOV R7, #0x7ffffffd
MOV R8, #0x80000000
MOV R9, #0x3
MOV R10, #0x7ffffffe
MOV R11, #0x2
MOV R12, #0x80000002
MOV R13, #0xfffffffe
MOV R14, #0x80000003


CMN R12	, R1,	ROR #19
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
