MOV R0, #0
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x6f
ADD R0, R0, #0x8900
ADD R0, R0, #0x250000
ADD R0, R0, #0x6e000000
MOV R1, #0x79
ADD R1, R1, #0x7500
ADD R1, R1, #0x930000
ADD R1, R1, #0xdc000000
MOV R2, #0x28
ADD R2, R2, #0xea00
ADD R2, R2, #0x970000
ADD R2, R2, #0x3b000000
MOV R3, #0x21
ADD R3, R3, #0x2a00
ADD R3, R3, #0x70000
ADD R3, R3, #0x64000000
MOV R4, #0x3a
ADD R4, R4, #0xc300
ADD R4, R4, #0x9f0000
ADD R4, R4, #0x50000000
MOV R5, #0x8c
ADD R5, R5, #0x7100
ADD R5, R5, #0x760000
ADD R5, R5, #0x6e000000
MOV R6, #0x43
ADD R6, R6, #0x8300
ADD R6, R6, #0xe40000
ADD R6, R6, #0x62000000
MOV R7, #0x30
ADD R7, R7, #0x8600
ADD R7, R7, #0x950000
ADD R7, R7, #0xea000000
MOV R8, #0x9e
ADD R8, R8, #0xdb00
ADD R8, R8, #0x170000
ADD R8, R8, #0x1b000000
MOV R9, #0x6d
ADD R9, R9, #0x4f00
ADD R9, R9, #0x8f0000
ADD R9, R9, #0x9000000
MOV R10, #0x47
ADD R10, R10, #0x1200
ADD R10, R10, #0x880000
ADD R10, R10, #0x94000000
MOV R11, #0x7
ADD R11, R11, #0x8500
ADD R11, R11, #0xaf0000
ADD R11, R11, #0x38000000
MOV R12, #0x10
ADD R12, R12, #0x1700
ADD R12, R12, #0xaa0000
ADD R12, R12, #0x13000000
MOV R13, #0x2e
ADD R13, R13, #0xae00
ADD R13, R13, #0x4b0000
ADD R13, R13, #0xe9000000
MOV R14, #0x44
ADD R14, R14, #0x2d00
ADD R14, R14, #0x120000
ADD R14, R14, #0x20000000


MOV R0, #0
BGE TARGET
MOV R0, #1
TARGET ADD R0, R0, #1

MOV R13, #0x200
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
