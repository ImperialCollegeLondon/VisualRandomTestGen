MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x89
ADD R0, R0, #0xee00
ADD R0, R0, #0x310000
ADD R0, R0, #0x6a000000
MOV R1, #0xdd
ADD R1, R1, #0xc700
ADD R1, R1, #0xf70000
ADD R1, R1, #0xac000000
MOV R2, #0x5b
ADD R2, R2, #0xfd00
ADD R2, R2, #0x8c0000
ADD R2, R2, #0x57000000
MOV R3, #0x9a
ADD R3, R3, #0x9300
ADD R3, R3, #0x6a0000
ADD R3, R3, #0xdf000000
MOV R4, #0xc7
ADD R4, R4, #0x5100
ADD R4, R4, #0x3b0000
ADD R4, R4, #0xdf000000
MOV R5, #0x99
ADD R5, R5, #0xaf00
ADD R5, R5, #0x6c0000
ADD R5, R5, #0x93000000
MOV R6, #0x82
ADD R6, R6, #0x5000
ADD R6, R6, #0x9d0000
ADD R6, R6, #0x74000000
MOV R7, #0xd2
ADD R7, R7, #0x4000
ADD R7, R7, #0xcc0000
ADD R7, R7, #0xe1000000
MOV R8, #0xcb
ADD R8, R8, #0x9800
ADD R8, R8, #0x9d0000
ADD R8, R8, #0xb000000
MOV R9, #0x8a
ADD R9, R9, #0x1100
ADD R9, R9, #0x2b0000
ADD R9, R9, #0x4b000000
MOV R10, #0x9f
ADD R10, R10, #0xc100
ADD R10, R10, #0xb20000
ADD R10, R10, #0x26000000
MOV R11, #0x5
ADD R11, R11, #0x400
ADD R11, R11, #0xf20000
ADD R11, R11, #0x32000000
MOV R12, #0xd5
ADD R12, R12, #0x5100
ADD R12, R12, #0xc30000
ADD R12, R12, #0xaf000000
MOV R13, #0xca
ADD R13, R13, #0xae00
ADD R13, R13, #0xea0000
ADD R13, R13, #0x5f000000
MOV R14, #0x97
ADD R14, R14, #0x7700
ADD R14, R14, #0xf20000
ADD R14, R14, #0xf5000000


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
