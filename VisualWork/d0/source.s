MOV R0, #0x7FFFFFFF
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x2
MOV R1, #0x55
ADD R1, R1, #0x8400
ADD R1, R1, #0xea0000
ADD R1, R1, #0xc7000000
MOV R2, #0x79
ADD R2, R2, #0xfd00
ADD R2, R2, #0x730000
ADD R2, R2, #0xc7000000
MOV R3, #0x56
ADD R3, R3, #0x2100
ADD R3, R3, #0x20000
ADD R3, R3, #0xb000000
MOV R4, #0xa6
ADD R4, R4, #0x6200
ADD R4, R4, #0x680000
ADD R4, R4, #0x11000000
MOV R5, #0x18
ADD R5, R5, #0xaf00
ADD R5, R5, #0x620000
ADD R5, R5, #0x1a000000
MOV R6, #0x7900
ADD R6, R6, #0x9c0000
ADD R6, R6, #0x69000000
MOV R7, #0x42
ADD R7, R7, #0xb500
ADD R7, R7, #0xf10000
ADD R7, R7, #0x74000000
MOV R8, #0xee
ADD R8, R8, #0x9f00
ADD R8, R8, #0x850000
ADD R8, R8, #0x96000000
MOV R9, #0x8e
ADD R9, R9, #0xcb00
ADD R9, R9, #0x970000
ADD R9, R9, #0xc8000000
MOV R10, #0x17
ADD R10, R10, #0x2700
ADD R10, R10, #0xaf0000
ADD R10, R10, #0xaa000000
MOV R11, #0x1a
ADD R11, R11, #0x4b00
ADD R11, R11, #0x1a0000
ADD R11, R11, #0x15000000
MOV R12, #0x9c
ADD R12, R12, #0x3200
ADD R12, R12, #0xc30000
ADD R12, R12, #0x22000000
MOV R13, #0x6c
ADD R13, R13, #0xc200
ADD R13, R13, #0x920000
ADD R13, R13, #0x75000000
MOV R14, #0x7a
ADD R14, R14, #0x8000
ADD R14, R14, #0x960000
ADD R14, R14, #0x9f000000


ADR	R9	,DAT3
LDR R2 ,[ R9	, #0x3 ]
DAT1 DCD	4162507893	,2051652965	,	875628451	,2327902103 ,4143343523	,4252415827	,	163375486, 867378097 ,1381391956 ,	1673852289, 2499209612 ,	3793465346	,3798599458 , 1373471335	,	1658201540,699090584 ,1747953959
DAT2	DCD	1093665824	,891173988, 1472179504,	261313777,3474773814, 794284477	,2675503718 , 3476880956	,	1622396859,	3237129200 , 1958874765,4065970759	,3673828622 , 3049588825	,	2495729969 ,1039709918,1327718549
DAT3 DCD 2823630739, 4002551282 ,110474899,	3857530885 , 443484901,	2158629036 ,	1739218806 ,	3005899930 ,	3306183151,	2242231358,	878981794,	298216151	, 1634282160 ,	4150986554, 3224370667	,2571008120 ,2191081186
DAT4 DCD	1243193257	, 262752961 , 3374647902	,3322033341 ,	4095666035 , 1235012173	,3311687246, 1429090342 ,203482120, 825791205,741373354	, 499942331, 3028868068,	3207126707	,2144672900,1906772018 ,	2383620295

MOV R13, #0x200
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
