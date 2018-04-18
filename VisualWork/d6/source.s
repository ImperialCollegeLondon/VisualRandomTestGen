MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0x1
MOV R1, #0xffffffff
MOV R2, #0x4
MOV R3, #0x2
MOV R4, #0xffffffff
MOV R5, #0xfffffffe
MOV R6, #0x1
MOV R7, #0xfffffffd
MOV R8, #0x1
MOV R9, #0xfffffffe
MOV R10, #0xfffffffc
MOV R11, #0x2
MOV R12, #0xffffffff
MOV R13, #0x0
MOV R14, #0xffffffff


MOVS	R3 ,	R1	,RRX
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
