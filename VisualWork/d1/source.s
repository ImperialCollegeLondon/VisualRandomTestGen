MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0x91
ADD R0, R0, #0x8700
ADD R0, R0, #0xfe0000
ADD R0, R0, #0x78000000
MOV R1, #0x2
MOV R2, #0x46
ADD R2, R2, #0xff00
ADD R2, R2, #0x6f0000
ADD R2, R2, #0x1a000000
MOV R3, #0x1
ADD R3, R3, #0x7600
ADD R3, R3, #0xbe0000
ADD R3, R3, #0xf9000000
MOV R4, #0x20
ADD R4, R4, #0x1c00
ADD R4, R4, #0xe30000
ADD R4, R4, #0x4d000000
MOV R5, #0xb9
ADD R5, R5, #0xc900
ADD R5, R5, #0x990000
ADD R5, R5, #0xe8000000
MOV R6, #0x5c
ADD R6, R6, #0x4b00
ADD R6, R6, #0xb10000
ADD R6, R6, #0x9f000000
MOV R7, #0x12
ADD R7, R7, #0x5900
ADD R7, R7, #0x290000
ADD R7, R7, #0x38000000
MOV R8, #0x36
ADD R8, R8, #0xbd00
ADD R8, R8, #0xc40000
ADD R8, R8, #0x3b000000
MOV R9, #0x79
ADD R9, R9, #0xca00
ADD R9, R9, #0x9e0000
ADD R9, R9, #0xdd000000
MOV R10, #0x9c
ADD R10, R10, #0xba00
ADD R10, R10, #0x20000
ADD R10, R10, #0x9d000000
MOV R11, #0x32
ADD R11, R11, #0x9300
ADD R11, R11, #0x4f0000
ADD R11, R11, #0xb4000000
MOV R12, #0x9d
ADD R12, R12, #0x4900
ADD R12, R12, #0xe70000
ADD R12, R12, #0x3b000000
MOV R13, #0xd1
ADD R13, R13, #0x3600
ADD R13, R13, #0xab0000
ADD R13, R13, #0xca000000
MOV R14, #0x3b
ADD R14, R14, #0xcf00
ADD R14, R14, #0x6e0000
ADD R14, R14, #0xfe000000


ADR R9,	DAT3
LDR R7	, [ R9	]
DAT1	DCD	4167975495 ,	2299728876,	2573314070,	167403437,	1240403591 ,2017392471 , 74172094, 2848938144,	2689761188, 3376534333 ,4063472366	, 1035200128 ,	3588964988	,2620202849	,3529388559	,2545730832 ,1546208477
DAT2	DCD 788595172	,	3798508002 ,	1394857617	,	2169407685 ,	1754680619,	3335108836	,1501192318	, 1903587797	,	3837800610	, 3179463753,664030591 , 76849810	,	1040209636 , 199813785 , 2370705375, 1992995629, 2603466638
DAT3 DCD 2545723370, 1983381104	,	746713776,1374579054	,1113059463 ,532028678,	3909237679 , 2917884041 ,1296983834	, 126719967 ,491381121 , 4249894869 ,	1675820095	,	3400111797 , 4218900663,	3596686236	, 498571756
DAT4 DCD 3368143243 ,	4221429328 , 69323474	, 1918615078 ,	187110412,	2195398905	,	1843580985,1018047684, 3398504367,	2879325005 ,	2722477627 ,2862167649,2592944193, 671529785	,	1844965792 ,	438635546 ,4283387670

MOV R13, #0x200
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
