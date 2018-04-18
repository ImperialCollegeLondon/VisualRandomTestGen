MOV R0, #0x7FFFFFFF
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x3
MOV R1, #0xffffffff
MOV R2, #0xfffffffc
MOV R3, #0x4
MOV R4, #0x15
MOV R5, #0xffffffff
MOV R6, #0x3
MOV R7, #0xffffffff
MOV R8, #0xfffffffc
MOV R9, #0x4
MOV R10, #0x3
MOV R11, #0xfffffffe
MOV R12, #0x1
MOV R13, #0x4
MOV R14, #0x2


MVNS	R8, R14 ,ROR	R4
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
