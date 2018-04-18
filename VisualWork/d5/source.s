MOV R0, #0x7FFFFFFF
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0xfffffffd
MOV R1, #0x2
MOV R2, #0x1
MOV R3, #0x4
MOV R4, #0xfffffffc
MOV R5, #0x0
MOV R6, #0x1
MOV R7, #0x4
MOV R8, #0x0
MOV R9, #0x1
MOV R10, #0xfffffffc
MOV R11, #0x3
MOV R12, #0xfffffffe
MOV R13, #0x4
MOV R14, #0xfffffffc


MVN R1	,R7	, ASR #31
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
