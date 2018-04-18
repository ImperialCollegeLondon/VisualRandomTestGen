MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0xfffffffc
MOV R1, #0x0
MOV R2, #0xffffffff
MOV R3, #0x4
MOV R4, #0xfffffffd
MOV R5, #0x0
MOV R6, #0x2
MOV R7, #0x1
MOV R8, #0x13
MOV R9, #0xfffffffe
MOV R10, #0xffffffff
MOV R11, #0x1
MOV R12, #0x3
MOV R13, #0xffffffff
MOV R14, #0xfffffffd


TST R5 , R5,ROR	R8
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
