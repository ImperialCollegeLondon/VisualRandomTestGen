namespace VisualTest

open System


module VTestSpec =

    open System
    open VCommon
    open EEExtensions
    open VTestDSL


 ///////////////////////////////////////////////////////////////////////////////////////////////////
 //
 //                                 CODE FOR COND BRANCH TESTING
 //                                 ----------------------------
 //
 ///////////////////////////////////////////////////////////////////////////////////////////////////

    let condBranchTests =
        let Conditions = ALLSTRINGS [ ""; "CC";"CS";"EQ";"NE";"HI";"HS";"LO";"LS";"GT";"GE";"LT";"LE";"VC";"VS"]
        ASMLINES [
            S "MOV R0, #0"
            S "B" ++ Conditions ++ WS ++ S "TARGET"
            S "MOV R0, #1"
            S "TARGET ADD R0, R0, #1"
            ]
        |> GENTESTS
    

    let computedBranchTests =
        ASMLINES [
            S "MOV R0, #0"
            S "ADR R1, TARGET"
            S "ADR R2, TARGETADDR"
            ALLSTRINGS [ "MOV PC, R1"; "MOV R1, PC"; "LDR PC, [R2]"; "STR PC, [R2]" ; "BL TARGET" ]
            S "ADD R0, R0, #1"
            S "ADD R0, R0, #1"
            S "TARGET ADD R0, R0, #1"
            S "ADD R0, R0, #1"
            S "ADD R0, R0, #1"
            S "LDR R3, [R2]"
            S "TARGETADDR DCD TARGET"
            ]
        |> GENTESTSZEROINIT
 ///////////////////////////////////////////////////////////////////////////////////////////////////
 //
 //                   CODE FOR ARM DP INSTRUCTION GENERATION
 //                   --------------------------------------
 //
 ///////////////////////////////////////////////////////////////////////////////////////////////////

    let dp3OpC = 
        ALLSTRINGS [ 
            "ADD";"ADC";"SUB";"SBC";"RSB";"RSC"; 
            "AND";"EOR";"BIC";"ORR" ; "ADDS"; 
            "ADCS"; "SBCS"; "RSBS"; "RSCS" ; "ANDS"; "EORS" ; "BICS"
        ]

    /// op-codes for 2 operand instructions to test    
    let dp2OpC =
        ALLSTRINGS [ "MOV"; "MVN"; "TST"; "TEQ"; "CMN"; "MOVS"; "MVNS" ]

    /// shift operands (without RRX)  
    let shiftOp = ALLSTRINGS [ "" ; "ASR" ; "LSR" ; "LSL" ; "ROR" ]

    /// shift operands (with RRX)
    let shiftOpRRX = ALLSTRINGS [ "" ; "RRX" ; "ASR" ; "LSR" ; "LSL" ; "ROR" ]

    /// generate a random DP immediate constant that is mostly of valid form
    let immediate() =
        let mask = (uint32 ((1L<<<32) - 1L))
        let rot (k:uint32) (n:int) = ((k >>> n) ||| (k <<< ((32-2*n)%32))) &&& mask
        let sh = randomT.Next(16)
        let k = uint32 (randomT.Next 256)
        let imm = rot k sh
        match randomT.Next(20) with
        | 0 -> ~~~imm
        | 1 -> uint32(-(int imm))
        | _ -> imm
        |> (fun n -> n &&& mask)


    /// set of registers to use for DP instructions (not R13 or R15) 
    let dpRegNum() = match randomT.Next(14) with | 13 -> 14 | n -> n


    /// TestCode for DP-valid register name string
    let DPREG = RAND <| fun () -> S (sprintf "R%d" (dpRegNum()))

    /// TestCode for random DP immediate value starting with #
    let IMM = RAND <| fun () -> S (sprintf "#%d" (int (immediate())))

    // let IMM = RAND <| fun () -> () |> immediate |> int |> sprintf "#%d" |>  S ?

    let SHIFTAMT = 
        RAND <| fun () ->         
            match randomT.Next(20) with
            | n when n < 10 -> 
                let r = dpRegNum()
                let reg = sprintf "R%d" r
                BOUNDED( WS ++ S reg, [r,Lim(0u,32u)])
            | _ -> WS ++ RAND (fun () -> randomT.Next(0,33) |> sprintf "#%d" |> S)

    let SHIFT =
        let mapF so =
            match so with
            | S "" -> S ""
            | S "RRX" -> WSQ ++ S "," ++ WSQ ++ S "RRX"
            | S s -> WS ++ S "," ++ WSQ ++ S s ++ SHIFTAMT
            | _ -> failwithf "Unexpected test description found in makeshift map function"
        shiftOpRRX |> TESTMAP mapF

    /// evaluate a TestCode using default register limits corresponding to DP corner cases
    /// these are over-ridden by any limits in the specification
    let DPGENTESTS dpLim = genTests randomFlags (fun _ -> dpRand dpLim ())

    let opImm() =  WSQ ++ S "," ++ WSQ ++ IMM
    let opShift() = WSQ ++ S "," ++ WSQ ++ DPREG ++ SHIFT
    let op2() = WSQ ++ S "," ++ DPREG
    let makeDPOp opc ops = DPGENTESTS 4 <|  opc ++ WS ++ DPREG ++ ops

    let dp3Shifts = makeDPOp dp3OpC  (op2()  ++ opShift())
    let dp3Imms = makeDPOp dp3OpC (op2() ++ opImm())
    let dp2Shifts = makeDPOp dp2OpC (WSQ ++ opShift())
    let dp2Imms = makeDPOp dp2OpC (opImm())



//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//                                                MEMORY COMMON CODE
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /// TestCode for ',' separator with opt white space
    let SEP = WSQ ++ S "," ++ WSQ

    /// TestCode for open '[' with opt white space after
    let BRA = S "[" ++ WSQ

    /// TestCode for close ']' with opt white space before
    let KET = WSQ ++ S "]" 

    /// TestCode for register n
    let RG (n:int) = S (sprintf "R%d" n)

    /// TestCode for a DCD with numb random data words
    let makeDCD lab (numb:int) = 
                S lab ++ WS ++ S "DCD"  ++ WS ++ (
                    [0..numb-1] 
                    |> List.map (fun _ -> uiAllRand() |> sprintf "%d" |> S  )
                    |> List.reduce (fun a b -> a ++ SEP ++ b))

    /// Choose a random number from pool.
    /// Return chosen number, rest of pool
    let chooseReg (pool: int list) = 
        let chosen = pool.[randomT.Next pool.Length]
        let pool' = List.filter ((<>) chosen) pool
        (chosen, pool')

    /// Choose a random number - not 13 - from pool
    /// return chosen number paired with rest of pool
    let chooseNo13Reg pool =
        let poolNo13 = List.filter ((<>) 13) pool
        let chosen = poolNo13.[randomT.Next poolNo13.Length]
        let pool' = List.filter ((<>) chosen) pool
        (chosen, pool')
    
    /// convert a choose function to
    /// one operating on a chosen list * pool monad
    /// so that multiple register choices can conveniently
    /// be chained
    let liftChoose chooseFun (chosenLst, pool) =
        let choice, pool' = chooseFun pool
        chosenLst @ [choice], pool'

    /// convert chosen list to a 2-tuple
    let choose2 choosers pool =
                match ([],pool) |> choosers with
                | [a;b], p -> a,b,p
                | _ -> failwith "What - can't happen!"

    /// convert chosen list to a 3-tuple
    let choose3 choosers pool =
                match ([],pool) |> choosers with
                | [a;b;c], p -> a,b,c,p
                | _ -> failwith "What - can't happen!"

    /// convert chosen list to a 4-tuple
    let choose4 choosers pool =
            match ([], pool) |> choosers with
            | [a;b;c;d], p -> a,b,c,d,p
            | _ -> failwith "What - can't happen!"

    /// convert chosen list to a 5-tuple
    let choose5 choosers pool =
        match ([], pool) |> choosers with
        | [a;b;c;d;e], p -> a,b,c,d,e,p
        | _ -> failwith "What - can't happen!"

    /// convert chosen list to a 6-tuple
    let choose6 choosers pool =
        match ([], pool) |> choosers with
        | [a;b;c;d;e;f], p -> a,b,c,d,e,f,p
        | _ -> failwith "What - can't happen!"

    /// choose a register
    let CR = liftChoose chooseReg

    /// choose a register other than R13
    let CRN13 = liftChoose chooseNo13Reg            
    
    /// TestCode of 4 DCDs defining 64 random initialised 
    /// words of memory good for testing memory instructions
    let makeDCDs() =
        ASMLINES [
            makeDCD "DAT1" 16  
            makeDCD "DAT2" 16
            makeDCD "DAT3" 16 
            makeDCD "DAT4" 16
        ]

    /// Make as a TestCode DCDs and also the code that Checksums them.
    /// pool -> list of free registers from which the 3 registers 
    /// used by the check code are chosen. This is normally the last
    /// lines of a memory test TC
    let makeMemCheckCode pool =
        let  rCheck1, rCheck2, rTmp, _ = choose3 (CR >> CR >> CRN13) pool
        ASMLINES [
            S <| sprintf "; **** CheckSum in R%d ***" rCheck1
            S <| sprintf "ADR R%d, DAT1+0x100" rCheck2
            S <| sprintf "MOV R%d, #0" rCheck1
            S <| sprintf "CHECKLOOP LDR R%d, [R%d,#-4]!" rTmp rCheck2
            S <| sprintf "ADD R%d, R%d, R%d" rCheck1 rCheck1 rTmp
            S <| sprintf "ADR R%d, DAT1" rTmp
            S <| sprintf "CMP R%d,R%d" rCheck2 rTmp
            S <| "BNE CHECKLOOP"
            makeDCDs()
            ]

    let setMBase mBase = S "ADR" ++ WS ++ RG mBase ++ SEP ++ S "DAT3"             

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//                            MEMORY SINGLE REGISTER LOAD/STORE INSTRUCTIONS
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                   
 
    /// this must be the same as for that used by Visual 
    /// (ensured programmatically by this project) 
    /// and that used by Visual2 (no guarantee)
    let dataSectionStart = 0x200u




    //   Could use:   let FTOTC f = (fun _ -> f () |> S) |> RAND 
    
    /// TestCode representing random DP immediates
    /// in hex or decimal (add binary?)
    let mImm =
        /// choose a suitable random immediate integer for memory offset testing
        let makeMemImm() = 
            let imms = [ 1;3;-1;-5;8;12;16;20;-8;-12;-16;-20]
            imms.[randomT.Next(imms.Length)]

        S "#" ++ 
            RANDTC [ 
                RAND <|  fun () -> S (sprintf "0x%x" (makeMemImm()))
                RAND <| fun () -> S (sprintf "%d" (makeMemImm()))
            ]

    /// TestCode representing random immediate scaling shift specification
    /// applied to register rx
    /// The string for the immediate is returned and a compatible bound on rx
    /// that ensures the overall offset stays within limits.
    let mShiftAmt rx = RAND <|  fun () ->
        let scaler = randomT.Next(1,4)
        let bnd = match scaler with
                  | 1 -> 8
                  | 2 -> 4
                  | _ -> 1
        let r = randomT.Next(10,14)
        BOUNDED (S <| sprintf "#%d" scaler, [rx, Lim(1u , uint32 bnd)])
        
    
    /// TestCode for the stuff inside [] in a LDR/STR
    let inSide r rx = 
        FORALL [
            RG r 
            RG r ++ SEP ++ mImm  
            RG r ++ SEP ++ RG rx
            RG r ++ SEP ++ RG rx ++ SEP ++ FORALL [S "LSL" ; S "LSR"; S "ASR"] ++ WS ++ mShiftAmt rx
        ]

    /// TestCode for the second operand (determining memory mode)
    /// in an LDR/STR
    let memMode r rx = 
        FORALL [  
            BRA ++ inSide r rx ++ KET ++ WSQ ++ ALLSTRINGS [ "" ; "!"]
            BRA ++ RG r ++ KET ++ SEP ++ mImm
        ]
    
    /// TestCode for a LDR/STR instruction test
    /// isLoad determined which
    let memSingleTest isLoad = 
        let memOpCodes = ALLSTRINGS ["LDR" ; "STR"]
        let memOpSuffs = ALLSTRINGS ["";"B"]
        let regPool = [0..14]
        let mData, mBase, mExtra, pool' = choose3 (CR >> CR >> CR) regPool
        /// TestCode for a single random LDR(B)/STR(B) instruction
        let memLoadsStores isLoad mData mBase mExtra = 
            let baseOpCode = if isLoad then "LDR" else "STR"
            S baseOpCode ++ ALLSTRINGS [ "" ; "B"] ++ WS ++ RG mData ++ SEP ++ (memMode mBase mExtra)
            |> fun tc -> BOUNDED( tc, [mExtra, Lim(1u,3u)])
        GENTESTS <|
            ASMLINES [ 
                setMBase mBase
                memLoadsStores isLoad mData mBase mExtra  
                (if isLoad then makeDCDs() else makeMemCheckCode pool')
            ] 

/////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//                             MULTIPLE REGISTER MEMORY INSTRUCTIONS
//                             -------------------------------------
//
/////////////////////////////////////////////////////////////////////////////////////////////////////////

    /// TestCode for a multiple register memory instruction test
    let memMultiTest isLoad =

        /// TestCode for a single randomly generated LDM/STM
        /// with an ADR that sets the base register to a suitable
        /// value to ensure memory address is within limits.
        let memLDMSTM isLoad mBase md1 md2 = 
            let twoRegs = RG md1 ++ SEP ++ RG md2
            let regRange = 
                // get ascending range:
                let range = List.sort [md1 ; md2]
                // check range is OK for use
                match range.[0], range.[1] with
                | a,b when mBase > a && mBase < b -> twoRegs // can't use this
                | a,b when b-a > 4 -> twoRegs // range may overlap mem checking - non-optimal
                | a, b -> RG a ++ S "-" ++ RG b
            let memRegList = RANDTC [ twoRegs ; regRange ]
            let multiMemSuffixes = ALLSTRINGS ["IB";"IA";"DB";"DA";"EA";"ED";"FA";"FD"]
            let opCodeBase = if isLoad then "LDM" else "STM"
            ASMLINES [
                setMBase mBase
                S opCodeBase ++ multiMemSuffixes ++ WS ++ RG mBase ++ WSQ ++
                    RANDSTRINGS ["" ; "!"] ++ SEP ++ S "{" ++ memRegList ++ S "}"
                ]

        GENTESTS <|
            let regPool = [0..14]
            let mData1,mData2,mBase,pool' = choose3 (CRN13 >> CRN13 >> CR) regPool
            ASMLINES [ 
                memLDMSTM isLoad mBase mData1 mData2 
                makeMemCheckCode pool'
            ]

