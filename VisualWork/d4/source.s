MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x1
MOV R1, #0x2
MOV R2, #0x3
MOV R3, #0x3
MOV R4, #0xfffffffd
MOV R5, #0xfffffffc
MOV R6, #0xfffffffd
MOV R7, #0x1
MOV R8, #0xffffffff
MOV R9, #0x3
MOV R10, #0xffffffff
MOV R11, #0xfffffffe
MOV R12, #0xfffffffc
MOV R13, #0x0
MOV R14, #0x0


MVN	R11	, R3, LSR	#28
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
