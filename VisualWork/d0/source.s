MOV R0, #0xFFFFFFFF
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0x3
MOV R1, #0xffffffff
MOV R2, #0xfffffffd
MOV R3, #0x4
MOV R4, #0xfffffffc
MOV R5, #0x1
MOV R6, #0x0
MOV R7, #0xfffffffd
MOV R8, #0xfffffffe
MOV R9, #0x1
MOV R10, #0xffffffff
MOV R11, #0xffffffff
MOV R12, #0x4
MOV R13, #0xfffffffc
MOV R14, #0x4


MOV	R12 ,R2
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
