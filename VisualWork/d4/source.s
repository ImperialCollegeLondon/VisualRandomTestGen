MOV R0, #0
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x79
ADD R0, R0, #0x7300
ADD R0, R0, #0x580000
ADD R0, R0, #0xc9000000
MOV R1, #0xa
ADD R1, R1, #0x9f00
ADD R1, R1, #0x5a0000
ADD R1, R1, #0xa9000000
MOV R2, #0x40
ADD R2, R2, #0x700
ADD R2, R2, #0x3a0000
ADD R2, R2, #0x3e000000
MOV R3, #0x86
ADD R3, R3, #0x1d00
ADD R3, R3, #0xee0000
ADD R3, R3, #0x30000000
MOV R4, #0x38
ADD R4, R4, #0x1300
ADD R4, R4, #0xc80000
ADD R4, R4, #0x88000000
MOV R5, #0xf1
ADD R5, R5, #0x1700
ADD R5, R5, #0xa40000
ADD R5, R5, #0x36000000
MOV R6, #0x59
ADD R6, R6, #0x4500
ADD R6, R6, #0xb90000
ADD R6, R6, #0xeb000000
MOV R7, #0x40
ADD R7, R7, #0xdb00
ADD R7, R7, #0xcb0000
ADD R7, R7, #0x3a000000
MOV R8, #0xd2
ADD R8, R8, #0xc300
ADD R8, R8, #0x550000
ADD R8, R8, #0xf1000000
MOV R9, #0x2d
ADD R9, R9, #0x1900
ADD R9, R9, #0xf40000
ADD R9, R9, #0x62000000
MOV R10, #0xd7
ADD R10, R10, #0x9300
ADD R10, R10, #0x4d0000
ADD R10, R10, #0x42000000
MOV R11, #0x44
ADD R11, R11, #0x9700
ADD R11, R11, #0x780000
ADD R11, R11, #0xe6000000
MOV R12, #0xc1
ADD R12, R12, #0x2500
ADD R12, R12, #0x980000
ADD R12, R12, #0xee000000
MOV R13, #0x37
ADD R13, R13, #0xa600
ADD R13, R13, #0xdd0000
ADD R13, R13, #0x6d000000
MOV R14, #0x57
ADD R14, R14, #0xed00
ADD R14, R14, #0xc90000
ADD R14, R14, #0xb8000000


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
