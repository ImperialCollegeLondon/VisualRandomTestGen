MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x3
MOV R1, #0x3
MOV R2, #0xfffffffc
MOV R3, #0x1
MOV R4, #0xfffffffe
MOV R5, #0xfffffffd
MOV R6, #0x0
MOV R7, #0x3
MOV R8, #0xfffffffd
MOV R9, #0xfffffffd
MOV R10, #0x1
MOV R11, #0xfffffffc
MOV R12, #0x2
MOV R13, #0x1
MOV R14, #0x3


MVN R14,R2
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
