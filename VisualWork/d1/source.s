MOV R0, #0
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0xfffffffc
MOV R1, #0xfffffffe
MOV R2, #0x0
MOV R3, #0xfffffffe
MOV R4, #0xfffffffc
MOV R5, #0xffffffff
MOV R6, #0xfffffffe
MOV R7, #0x2
MOV R8, #0xfffffffe
MOV R9, #0x1
MOV R10, #0x4
MOV R11, #0xfffffffe
MOV R12, #0x2
MOV R13, #0x1
MOV R14, #0xfffffffe


MVNS R10,R2	,	ASR R9
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
