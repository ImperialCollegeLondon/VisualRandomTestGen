MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x4
MOV R1, #0x1
MOV R2, #0x3
MOV R3, #0xfffffffe
MOV R4, #0xfffffffc
MOV R5, #0x2
MOV R6, #0xffffffff
MOV R7, #0xfffffffc
MOV R8, #0x3
MOV R9, #0xfffffffd
MOV R10, #0xffffffff
MOV R11, #0x2
MOV R12, #0xfffffffc
MOV R13, #0x4
MOV R14, #0x2


TEQ	R4 , R5 ,LSL	#1
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
