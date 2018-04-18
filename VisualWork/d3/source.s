MOV R0, #0xFFFFFFFF
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0x3
MOV R1, #0xfffffffd
MOV R2, #0x3
MOV R3, #0xfffffffe
MOV R4, #0x1
MOV R5, #0x3
MOV R6, #0xfffffffe
MOV R7, #0xffffffff
MOV R8, #0x3
MOV R9, #0xfffffffe
MOV R10, #0xfffffffd
MOV R11, #0x4
MOV R12, #0x4
MOV R13, #0xfffffffe
MOV R14, #0x4


MVNS	R4, R11,	ASR	#27
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
