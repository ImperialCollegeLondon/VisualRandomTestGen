MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0xe1
ADD R0, R0, #0xb600
ADD R0, R0, #0xeb0000
ADD R0, R0, #0x34000000
MOV R1, #0xf1
ADD R1, R1, #0x7c00
ADD R1, R1, #0xf30000
ADD R1, R1, #0x1d000000
MOV R2, #0xc8
ADD R2, R2, #0x7e00
ADD R2, R2, #0xba0000
ADD R2, R2, #0x5b000000
MOV R3, #0x51
ADD R3, R3, #0x5300
ADD R3, R3, #0xfe0000
ADD R3, R3, #0xe7000000
MOV R4, #0x95
ADD R4, R4, #0x8800
ADD R4, R4, #0xdb0000
ADD R4, R4, #0x89000000
MOV R5, #0x9e00
ADD R5, R5, #0x760000
ADD R5, R5, #0x98000000
MOV R6, #0x5b
ADD R6, R6, #0x9600
ADD R6, R6, #0x380000
ADD R6, R6, #0x1d000000
MOV R7, #0xfd
ADD R7, R7, #0x7500
ADD R7, R7, #0x760000
ADD R7, R7, #0xd2000000
MOV R8, #0x32
ADD R8, R8, #0x5500
ADD R8, R8, #0xd70000
ADD R8, R8, #0x71000000
MOV R9, #0xbe
ADD R9, R9, #0x9300
ADD R9, R9, #0x760000
ADD R9, R9, #0xea000000
MOV R10, #0xe
ADD R10, R10, #0x1000
ADD R10, R10, #0xab0000
ADD R10, R10, #0xb3000000
MOV R11, #0xd0
ADD R11, R11, #0x2a00
ADD R11, R11, #0x840000
ADD R11, R11, #0xda000000
MOV R12, #0xc7
ADD R12, R12, #0xd500
ADD R12, R12, #0x290000
ADD R12, R12, #0xf2000000
MOV R13, #0x15
ADD R13, R13, #0xeb00
ADD R13, R13, #0xca0000
ADD R13, R13, #0x55000000
MOV R14, #0x72
ADD R14, R14, #0xf300
ADD R14, R14, #0x940000
ADD R14, R14, #0xfc000000


MOV R0, #0
BGE	TARGET
MOV R0, #1
TARGET ADD R0, R0, #1

MOV R13, #0x200
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
