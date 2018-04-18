MOV R0, #0xFFFFFFFF
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0xfffffffc
MOV R1, #0x2
MOV R2, #0xfffffffe
MOV R3, #0x4
MOV R4, #0xffffffff
MOV R5, #0xfffffffe
MOV R6, #0xffffffff
MOV R7, #0x1
MOV R8, #0xfffffffd
MOV R9, #0x2
MOV R10, #0x2
MOV R11, #0xfffffffe
MOV R12, #0xfffffffd
MOV R13, #0xfffffffe
MOV R14, #0x0


MVNS R6,R1,	ROR	#32
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
