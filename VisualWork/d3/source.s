MOV R0, #0
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x4c
ADD R0, R0, #0xec00
ADD R0, R0, #0x570000
ADD R0, R0, #0xc8000000
MOV R1, #0xc9
ADD R1, R1, #0xbc00
ADD R1, R1, #0x7b000000
MOV R2, #0x94
ADD R2, R2, #0xee00
ADD R2, R2, #0xf70000
ADD R2, R2, #0x3c000000
MOV R3, #0xb3
ADD R3, R3, #0x100
ADD R3, R3, #0xf0000
ADD R3, R3, #0x95000000
MOV R4, #0xa2
ADD R4, R4, #0xb900
ADD R4, R4, #0x650000
ADD R4, R4, #0xa0000000
MOV R5, #0x16
ADD R5, R5, #0x4400
ADD R5, R5, #0x60000
ADD R5, R5, #0x41000000
MOV R6, #0x52
ADD R6, R6, #0x9c00
ADD R6, R6, #0x6f0000
ADD R6, R6, #0x95000000
MOV R7, #0x77
ADD R7, R7, #0x7b00
ADD R7, R7, #0x1b0000
ADD R7, R7, #0xdd000000
MOV R8, #0x75
ADD R8, R8, #0x7700
ADD R8, R8, #0x4e0000
ADD R8, R8, #0x8f000000
MOV R9, #0xea
ADD R9, R9, #0xcf00
ADD R9, R9, #0xe0000
ADD R9, R9, #0xe000000
MOV R10, #0xc7
ADD R10, R10, #0xf200
ADD R10, R10, #0x3d0000
ADD R10, R10, #0x27000000
MOV R11, #0xae
ADD R11, R11, #0xa00
ADD R11, R11, #0x830000
ADD R11, R11, #0xf6000000
MOV R12, #0xe5
ADD R12, R12, #0x3d00
ADD R12, R12, #0x290000
ADD R12, R12, #0x31000000
MOV R13, #0xd0
ADD R13, R13, #0x1000
ADD R13, R13, #0xc80000
ADD R13, R13, #0xfc000000
MOV R14, #0x21
ADD R14, R14, #0xf200
ADD R14, R14, #0xf10000
ADD R14, R14, #0xec000000


ADR	R7 ,	DAT3
STMFA R7 ! , {R2 , R5}

; **** CheckSum in R9 ***
ADR R10, DAT1+0x100
MOV R9, #0
CHECKLOOP LDR R6, [R10,#-4]!
ADD R9, R9, R6
ADR R6, DAT1
CMP R10,R6
BNE CHECKLOOP
DAT1	DCD 1203904488, 3825603838,916364210 ,	3637660514 ,3966551801	, 2724160989 ,	691225595 , 2584089571,4174799585 ,	1378304701, 2645881448	,	37809748 ,2737551133	,	1215408824 , 3283766723	,	2574974144
DAT2 DCD	763274655 ,	2928722724,	1785620640, 1460148855	,273928726	,306031654,1945418164,	3630955837 ,	1938566205 , 4131785672 ,	903936244	, 821929550	, 3366749234,331269928 ,	1821102860, 751829607
DAT3 DCD 2772766677 ,1111528489, 2661705762,98562848	, 2054793844,	2866755701, 1757493073, 2811090799	, 2731388875 ,	2850158581,3340240333	,	3398757894 , 1996678787 ,4222152475	, 2247347080	,4142911752
DAT4	DCD	3011828183,3908376022 ,3807099041	,	678712722,3934123196,2302586437 ,1497513269 , 486885039 ,448511576	,2389140202	, 2283426264 ,	3943209354, 3703420141 , 2846513856 ,806620459 ,1219123186



MOV R13, #0x200
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
