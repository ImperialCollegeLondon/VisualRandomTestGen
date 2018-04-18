MOV R0, #0x7FFFFFFF
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x3
MOV R1, #0xfffffffe
MOV R2, #0xfffffffe
MOV R3, #0x1
MOV R4, #0x3
MOV R5, #0xfffffffd
MOV R6, #0x0
MOV R7, #0x2
MOV R8, #0x3
MOV R9, #0xffffffff
MOV R10, #0xffffffff
MOV R11, #0x0
MOV R12, #0x2
MOV R13, #0x3
MOV R14, #0xfffffffc


CMN R12, R7 ,	RRX
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
