MOV R0, #0
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x3
MOV R1, #0x3
MOV R2, #0x0
MOV R3, #0xffffffff
MOV R4, #0xfffffffd
MOV R5, #0x0
MOV R6, #0x0
MOV R7, #0x0
MOV R8, #0xfffffffe
MOV R9, #0x2
MOV R10, #0xfffffffe
MOV R11, #0x2
MOV R12, #0xfffffffc
MOV R13, #0x2
MOV R14, #0x2


MOV	R6,R6 ,ASR	#0
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
