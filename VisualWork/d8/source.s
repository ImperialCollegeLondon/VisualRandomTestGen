MOV R0, #0xFFFFFFFF
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x3
MOV R1, #0x7ffffffc
MOV R2, #0x7ffffffd
MOV R3, #0x4
MOV R4, #0x1
MOV R5, #0x80000001
MOV R6, #0xfffffffd
MOV R7, #0x7ffffffc
MOV R8, #0x80000001
MOV R9, #0x2
MOV R10, #0xfffffffc
MOV R11, #0x7fffffff
MOV R12, #0xfffffffe
MOV R13, #0x80000002
MOV R14, #0x80000004


MVNS R6	, R8	,LSR #12
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
