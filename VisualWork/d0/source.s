MOV R0, #0
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0xfffffffe
MOV R1, #0x1
MOV R2, #0x3
MOV R3, #0x1
MOV R4, #0xfffffffc
MOV R5, #0x3
MOV R6, #0x1
MOV R7, #0x3
MOV R8, #0xe
MOV R9, #0x0
MOV R10, #0xfffffffd
MOV R11, #0xffffffff
MOV R12, #0x1
MOV R13, #0x0
MOV R14, #0x2


MVNS R14	,	R9	, ROR	R8
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
