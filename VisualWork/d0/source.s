MOV R0, #0x7FFFFFFF
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x8f
ADD R0, R0, #0x9800
ADD R0, R0, #0x240000
ADD R0, R0, #0xc0000000
MOV R1, #0x99
ADD R1, R1, #0x300
ADD R1, R1, #0xdf0000
ADD R1, R1, #0x78000000
MOV R2, #0x74
ADD R2, R2, #0x2900
ADD R2, R2, #0xa70000
ADD R2, R2, #0x7d000000
MOV R3, #0x55
ADD R3, R3, #0x1100
ADD R3, R3, #0x780000
ADD R3, R3, #0x38000000
MOV R4, #0xd
ADD R4, R4, #0x3200
ADD R4, R4, #0x7e0000
ADD R4, R4, #0xfb000000
MOV R5, #0x89
ADD R5, R5, #0xb400
ADD R5, R5, #0x3e0000
ADD R5, R5, #0xa000000
MOV R6, #0x3b
ADD R6, R6, #0x8f00
ADD R6, R6, #0x120000
ADD R6, R6, #0xc1000000
MOV R7, #0x3a
ADD R7, R7, #0xf400
ADD R7, R7, #0x910000
ADD R7, R7, #0x82000000
MOV R8, #0x4a
ADD R8, R8, #0xec00
ADD R8, R8, #0xea0000
ADD R8, R8, #0x9c000000
MOV R9, #0xf4
ADD R9, R9, #0x7b00
ADD R9, R9, #0x570000
ADD R9, R9, #0xd4000000
MOV R10, #0x6a
ADD R10, R10, #0x6100
ADD R10, R10, #0x400000
ADD R10, R10, #0x6b000000
MOV R11, #0x3c
ADD R11, R11, #0xec00
ADD R11, R11, #0xcb0000
ADD R11, R11, #0xee000000
MOV R12, #0xf5
ADD R12, R12, #0xd800
ADD R12, R12, #0x300000
ADD R12, R12, #0x5000000
MOV R13, #0xc5
ADD R13, R13, #0xfb00
ADD R13, R13, #0x7b0000
ADD R13, R13, #0xc7000000
MOV R14, #0x1f
ADD R14, R14, #0x8400
ADD R14, R14, #0x130000
ADD R14, R14, #0xc000000


ADR R0, DAT
LDMIA R0, {R1-R6}
LDR R7, =TESTEQU
DAT DCD 0x12345678
DAT1 DCB 3,4,5,6
DAT2 DCB 12,13,14,15,16,17,18,19
FILL 4
DAT4 DCD 111
TESTEQU EQU DAT+3

MOV R13, #0x200
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
