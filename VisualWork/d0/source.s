MOV R0, #0
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x2
MOV R1, #0x80000000
MOV R2, #0xffffffff
MOV R3, #0x7ffffffd
MOV R4, #0x80000002
MOV R5, #0x3
MOV R6, #0x80000003
MOV R7, #0x0
MOV R8, #0x7ffffffd
MOV R9, #0xfffffffc
MOV R10, #0xfffffffc
MOV R11, #0x7fffffff
MOV R12, #0x7fffffff
MOV R13, #0xfffffffc
MOV R14, #0x3


TEQ R10,R5
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
