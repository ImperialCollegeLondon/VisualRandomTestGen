MOV R0, #0xFFFFFFFF
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x81
ADD R0, R0, #0x4200
ADD R0, R0, #0xd50000
ADD R0, R0, #0xea000000
MOV R1, #0xdd
ADD R1, R1, #0x6200
ADD R1, R1, #0xf20000
ADD R1, R1, #0x62000000
MOV R2, #0xd
ADD R2, R2, #0x400
ADD R2, R2, #0xbf0000
ADD R2, R2, #0x18000000
MOV R3, #0x9e
ADD R3, R3, #0xf900
ADD R3, R3, #0xa50000
ADD R3, R3, #0x32000000
MOV R4, #0x67
ADD R4, R4, #0x8000
ADD R4, R4, #0xf30000
ADD R4, R4, #0x67000000
MOV R5, #0x1d
ADD R5, R5, #0xc500
ADD R5, R5, #0xb40000
ADD R5, R5, #0x89000000
MOV R6, #0x8a
ADD R6, R6, #0xdb00
ADD R6, R6, #0xfc0000
ADD R6, R6, #0x25000000
MOV R7, #0x48
ADD R7, R7, #0x1500
ADD R7, R7, #0x130000
ADD R7, R7, #0x30000000
MOV R8, #0x8b
ADD R8, R8, #0xa200
ADD R8, R8, #0x630000
ADD R8, R8, #0xda000000
MOV R9, #0xdc
ADD R9, R9, #0xb500
ADD R9, R9, #0xb80000
ADD R9, R9, #0x13000000
MOV R10, #0x7d
ADD R10, R10, #0xb00
ADD R10, R10, #0x630000
ADD R10, R10, #0x3f000000
MOV R11, #0xb2
ADD R11, R11, #0x8500
ADD R11, R11, #0x1b0000
ADD R11, R11, #0x3a000000
MOV R12, #0x58
ADD R12, R12, #0xd400
ADD R12, R12, #0x7e0000
ADD R12, R12, #0xe7000000
MOV R13, #0x1f
ADD R13, R13, #0xd700
ADD R13, R13, #0x160000
ADD R13, R13, #0xc2000000
MOV R14, #0x79
ADD R14, R14, #0x8000
ADD R14, R14, #0xb50000
ADD R14, R14, #0xbe000000


MOV R0, #0
ADR R1, DAT
MOV PC, R1
ADD R0, R0, #1
ADD R0, R0, #1
DAT ADD R0,R0,#1
ADD R0, R0, #1
ADD R0, R0, #1

MOV R13, #0x200
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
