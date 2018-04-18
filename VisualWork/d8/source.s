MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0xfffffffd
MOV R1, #0x0
MOV R2, #0xffffffff
MOV R3, #0xc
MOV R4, #0x3
MOV R5, #0x2
MOV R6, #0xfffffffe
MOV R7, #0x3
MOV R8, #0xfffffffd
MOV R9, #0x1
MOV R10, #0x4
MOV R11, #0x3
MOV R12, #0x0
MOV R13, #0x0
MOV R14, #0x0


MOV	R4	, R1	,LSL R3
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
