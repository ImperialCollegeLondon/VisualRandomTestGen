MOV R0, #0x7FFFFFFF
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x4
MOV R1, #0x7ffffffd
MOV R2, #0x80000004
MOV R3, #0x3
MOV R4, #0xfffffffc
MOV R5, #0x7ffffffd
MOV R6, #0xffffffff
MOV R7, #0x80000003
MOV R8, #0x80000004
MOV R9, #0x0
MOV R10, #0x0
MOV R11, #0x7ffffffc
MOV R12, #0x4
MOV R13, #0x7fffffff
MOV R14, #0x80000002


TST R9,R9	, ROR #4
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
