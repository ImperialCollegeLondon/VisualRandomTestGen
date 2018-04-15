MOV R0, #0xFFFFFFFF
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0xfffffffc
MOV R1, #0x80000000
MOV R2, #0x80000004
MOV R3, #0x2
MOV R4, #0x80000004
MOV R5, #0xffffffff
MOV R6, #0xfffffffe
MOV R7, #0x80000004
MOV R8, #0xfffffffe
MOV R9, #0x80000002
MOV R10, #0x2
MOV R11, #0x80000001
MOV R12, #0xffffffff
MOV R13, #0x7ffffffe
MOV R14, #0x7ffffffd


CMN R3 ,	R3,LSR #16
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
