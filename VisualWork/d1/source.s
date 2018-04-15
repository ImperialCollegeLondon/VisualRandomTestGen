MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x1
MOV R1, #0x7ffffffe
MOV R2, #0xfffffffe
MOV R3, #0x80000004
MOV R4, #0xffffffff
MOV R5, #0x80000000
MOV R6, #0x2
MOV R7, #0x7ffffffc
MOV R8, #0x0
MOV R9, #0x7ffffffd
MOV R10, #0x80000001
MOV R11, #0xfffffffd
MOV R12, #0x7ffffffd
MOV R13, #0x3
MOV R14, #0xffffffff


TST R11	,R9	,LSL #23
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
