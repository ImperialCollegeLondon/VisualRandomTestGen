MOV R0, #0xFFFFFFFF
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0xfffffffd
MOV R1, #0x4
MOV R2, #0x3
MOV R3, #0xfffffffc
MOV R4, #0x4
MOV R5, #0x0
MOV R6, #0x1
MOV R7, #0x0
MOV R8, #0xffffffff
MOV R9, #0xfffffffc
MOV R10, #0x1
MOV R11, #0x4
MOV R12, #0xfffffffe
MOV R13, #0xffffffff
MOV R14, #0x0


CMN R4 ,	R1	,	LSL #0
MOV R13, #0x1000
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
