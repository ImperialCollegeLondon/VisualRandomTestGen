MOV R0, #0xFFFFFFFF
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x7b
ADD R0, R0, #0x7700
ADD R0, R0, #0xb30000
ADD R0, R0, #0xa3000000
MOV R1, #0x2
MOV R2, #0x15
ADD R2, R2, #0xa400
ADD R2, R2, #0xb00000
ADD R2, R2, #0x80000000
MOV R3, #0x6a
ADD R3, R3, #0x7100
ADD R3, R3, #0x290000
ADD R3, R3, #0xa3000000
MOV R4, #0x72
ADD R4, R4, #0xe200
ADD R4, R4, #0xc00000
ADD R4, R4, #0x86000000
MOV R5, #0x55
ADD R5, R5, #0xd00
ADD R5, R5, #0xb10000
ADD R5, R5, #0x30000000
MOV R6, #0xbd
ADD R6, R6, #0x5000
ADD R6, R6, #0xd20000
ADD R6, R6, #0x5c000000
MOV R7, #0xab
ADD R7, R7, #0x2d00
ADD R7, R7, #0x780000
ADD R7, R7, #0x83000000
MOV R8, #0xc4
ADD R8, R8, #0x8a00
ADD R8, R8, #0x310000
ADD R8, R8, #0x72000000
MOV R9, #0xb6
ADD R9, R9, #0x6600
ADD R9, R9, #0x100000
ADD R9, R9, #0x18000000
MOV R10, #0x86
ADD R10, R10, #0x5400
ADD R10, R10, #0xed0000
ADD R10, R10, #0x4d000000
MOV R11, #0x47
ADD R11, R11, #0x1800
ADD R11, R11, #0x690000
ADD R11, R11, #0xac000000
MOV R12, #0xd2
ADD R12, R12, #0xdc00
ADD R12, R12, #0x7c0000
ADD R12, R12, #0x6000000
MOV R13, #0x8
ADD R13, R13, #0x2d00
ADD R13, R13, #0x760000
ADD R13, R13, #0xb3000000
MOV R14, #0x2d
ADD R14, R14, #0x9600
ADD R14, R14, #0xb20000
ADD R14, R14, #0xe4000000


ADR R9	,DAT3
LDR R7,	[ R9]	, #0xfffffff0
DAT1 DCD 4167975495,	2299728876,2573314070 ,167403437	,1240403591 , 2017392471,74172094,2848938144 , 2689761188 ,	3376534333 , 4063472366 ,	1035200128, 3588964988 , 2620202849	, 3529388559 , 2545730832	,	1546208477
DAT2 DCD	788595172,	3798508002,	1394857617, 2169407685	, 1754680619	, 3335108836 ,	1501192318 ,	1903587797,	3837800610	,3179463753,664030591 , 76849810	,1040209636,199813785 ,	2370705375,1992995629, 2603466638
DAT3	DCD	2545723370 ,1983381104	,	746713776	,1374579054	, 1113059463 ,532028678,	3909237679,2917884041 ,	1296983834	,	126719967	,	491381121,	4249894869,1675820095,3400111797, 4218900663	,	3596686236 ,498571756
DAT4	DCD 3368143243	, 4221429328 , 69323474 ,1918615078	, 187110412 ,2195398905, 1843580985 ,	1018047684 , 3398504367 ,	2879325005	, 2722477627 ,2862167649,	2592944193,671529785 , 1844965792	,438635546	,4283387670

MOV R13, #0x200
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
