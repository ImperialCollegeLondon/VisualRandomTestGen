MOV R0, #0
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0x2
MOV R1, #0xfffffffc
MOV R2, #0x2
MOV R3, #0x1
MOV R4, #0xfffffffc
MOV R5, #0x3
MOV R6, #0x3
MOV R7, #0xfffffffd
MOV R8, #0x4
MOV R9, #0x3
MOV R10, #0x1
MOV R11, #0x4
MOV R12, #0xfffffffc
MOV R13, #0x2
MOV R14, #0x4


MVNS	R3	, R5 ,	LSL	#10
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
