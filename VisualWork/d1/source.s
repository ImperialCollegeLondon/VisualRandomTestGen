
MOV R0, #0
ADR R1, TARGET
ADR R2, TARGETADDR
MOV R1, PC
ADD R0, R0, #1
ADD R0, R0, #1
TARGET ADD R0, R0, #1
ADD R0, R0, #1
ADD R0, R0, #1
LDR R3, [R2]
TARGETADDR DCD TARGET

MOV R13, #0x200
LDMIA R13, {R0-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
