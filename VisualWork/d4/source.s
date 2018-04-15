MOV R0, #0x7FFFFFFF
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0xfffffffe
MOV R1, #0x80000003
MOV R2, #0x3
MOV R3, #0x7ffffffe
MOV R4, #0x0
MOV R5, #0x80000000
MOV R6, #0xffffffff
MOV R7, #0x80000004
MOV R8, #0x80000001
MOV R9, #0xfffffffe
MOV R10, #0x3
MOV R11, #0x7fffffff
MOV R12, #0xfffffffd
MOV R13, #0x80000004
MOV R14, #0x80000002


MOV R8, R5 ,	LSL #20
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
