MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x91
ADD R0, R0, #0x200
ADD R0, R0, #0xb70000
ADD R0, R0, #0x3a000000
MOV R1, #0x30
ADD R1, R1, #0x6500
ADD R1, R1, #0x830000
ADD R1, R1, #0x3a000000
MOV R2, #0x2f
ADD R2, R2, #0xaa00
ADD R2, R2, #0xd20000
ADD R2, R2, #0x85000000
MOV R3, #0x83
ADD R3, R3, #0xe100
ADD R3, R3, #0x910000
ADD R3, R3, #0xee000000
MOV R4, #0x1c
ADD R4, R4, #0xb100
ADD R4, R4, #0xbf0000
ADD R4, R4, #0x87000000
MOV R5, #0xbb
ADD R5, R5, #0x8600
ADD R5, R5, #0x370000
ADD R5, R5, #0xa8000000
MOV R6, #0x60
ADD R6, R6, #0x3000
ADD R6, R6, #0xa50000
ADD R6, R6, #0x5c000000
MOV R7, #0x58
ADD R7, R7, #0x2500
ADD R7, R7, #0x6e0000
ADD R7, R7, #0xbb000000
MOV R8, #0xd
ADD R8, R8, #0x8800
ADD R8, R8, #0x2c0000
ADD R8, R8, #0x29000000
MOV R9, #0x69
ADD R9, R9, #0x5700
ADD R9, R9, #0xef0000
ADD R9, R9, #0xe3000000
MOV R10, #0x61
ADD R10, R10, #0xc600
ADD R10, R10, #0xfc0000
ADD R10, R10, #0x29000000
MOV R11, #0x53
ADD R11, R11, #0x5e00
ADD R11, R11, #0xfa0000
ADD R11, R11, #0x3c000000
MOV R12, #0x6
ADD R12, R12, #0x8700
ADD R12, R12, #0xb40000
ADD R12, R12, #0x60000000
MOV R13, #0xf4
ADD R13, R13, #0x6400
ADD R13, R13, #0xc20000
ADD R13, R13, #0xb000000
MOV R14, #0xcc
ADD R14, R14, #0x6200
ADD R14, R14, #0x200000
ADD R14, R14, #0x2e000000


MOV R0, #0
BGT TARGET
MOV R0, #1
TARGET ADD R0, R0, #1

MOV R13, #0x200
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
