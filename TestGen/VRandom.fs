namespace VisualTest

open System


module VRandom =

    open System
    open VCommon
    open EEExtensions
    open System.Security.Cryptography

    /// common RNG for all code
    /// this is built-in .Net and fast but not very good!
    /// (randomT.Next (a,b)) will return a random int in [a..b-1]
    /// (randomT.Next a) will return a random int in [0..a-1]
    /// (randomT.Next ())  will return a random non-negative int
    /// a,b must be non-negative!
    let randomT = System.Random()

    /// select a random element of a list
    let randomChoice (lst: 'a list) =
        lst.[randomT.Next(lst.Length)]


    /// limits random initial uint32 value
    /// used to set init values in registers for tests
    type RegBound = 
        | Lim of a:uint32 * b:uint32 /// range a..b
        | Val of v:uint32 /// single value v
        | DPB of maxDifference: uint32 /// +/- maxDifference from 0 or 0x80000000
    
    /// specification for random initial values of R0..R14
    /// unspecified registers will have unconstrained random values
    type RegSpec = (int * RegBound) list

    type TestRand =
        | RAND of (unit -> TestRand)
        | SR of string * RegSpec


    /// constructor for randomly generated assembler strings
    /// recursive definition allows register constraints to be linked to strings
    type TestCode =
        | TR of TestRand
        | FORALL of TestCode list

    let S s = TR (SR ( (s,[])))

    /// construct TestCode for exhaustive tests, one for each of the strings
    /// total number of tests will be product of all joined exhaustive tests
    /// Compare this with RANDSTRINGS
    let ALLSTRINGS lst = FORALL (lst |> List.map S)

    /// construct TestCode for a randomised test: every test containing this will be given a new
    /// randomly generated string from the list. compare this with ALLSTRINGS
    let RANDSTRINGS (lst: string list) = (fun () -> SR (lst.[randomT.Next(lst.Length)],[])) |> RAND |> TR

 
    let RANDTR (trList: TestRand list) = RAND <| fun () -> randomChoice trList
 
        


    let TESTMAP map tc =
        match tc with
        | FORALL lis -> List.map map lis |> FORALL
        | _ -> failwithf "Unexpected Test case in testMap %A" tc


    /// Return minB..maxB RegBounds for multiple registers
    let makeBounds rl minB maxB =
        rl |> List.map (fun n -> n, Lim(minB,maxB))

    /// Return 
    let makeRegTC (iLst: int list) = List.map (fun n -> "R" + n.ToString()) iLst

    let (|MATCH|_|) re s = String.regexMatch re s 

    let (|PARSEINT|_|) s = 
        match Int32.TryParse s with 
        | true , n -> Some n 
        | false , _ -> None
    
    /// Active Pattern to return reg int from reg string: R3 -> 3
    /// given non-reg name it does not match
    let rec (|REGNUM|_|) tc =
        match tc with
        | SR ("PC",_) -> Some 15
        | SR ("LR",_) -> Some 14
        | SR ("SP",_) -> Some 13
        | SR (MATCH "R([0-9])+" (PARSEINT n),_)  -> Some n
        | _ -> None
    
    /// Extract a list of register numbers from a TestCode
    /// e.g. FORALL [S "R10" ; S "R3"] -> [3 ; 10]
    /// Also works on single registers (S "R3")
    /// Will correctly recurse to inner FORALL & BOUNDED cases
    /// Will ignore registers inside other TestCode cases
    /// The reg number list is sorted in ascending order
    let rec fromRegTC (regLst: TestCode) = 
        match regLst with
        | TR (REGNUM n) -> [n]
        | TR _ -> []
        | FORALL rLst -> 
            rLst 
            |> List.collect fromRegTC
        | _ -> failwithf "Unexpected testcode for register number extraction: %A" regLst
        |> List.sort
     
 
    /// Combine two RegBound values to get their (set) intersection
    /// Only Val and Limit bounds can be intersected in general
    /// TODO: work out correct intersection for DPB bounds with all Lim bounds
    let rec intersectBounds b1 b2 =
        match b1,b2 with
        | Lim(x1,y1), Lim(x2,y2) when min y1 y2 >= max x1 x2-> Lim(max x1 x2, min y1 y2)
        | Lim _, Lim _  -> failwithf "Bad TestCode spec: RegBounds %A and %A cannot be combined because they have empty intersection" b1 b2
        | Val x, b -> intersectBounds (Lim(x,x)) b
        | DPB x, DPB y -> DPB (min x y)
        | DPB n, Lim(a,b) when a=UInt32.MinValue && b=UInt32.MaxValue -> DPB n
        | DPB n, Lim(a,b) when a=b && (abs (int a) <= int n || (abs (0x80000000 - int a) <= int n)) ->
            Lim(a,a)
        | DPB n, Lim(x2,y2) -> failwithf "Can't combine DP and Lim register bounds: %A, %A" b1 b2
        | b1, b2 -> intersectBounds b2 b1

    /// Combine s1 and s2 RegSpec concatenating lists and taking intersection of duplicate bounds
    let joinRegSpecs (s1:RegSpec) (s2:RegSpec) =
        let grb r (s:RegSpec) = 
            List.tryFind (fun (r',_) -> r = r') s
            |> Option.map (fun (r,b) -> b)
        let getRegBnd r =
            match grb r s1, grb r s2 with
            | None, None -> []
            | None, Some b1
            | Some b1, None -> [(r,b1)]
            | Some b1, Some b2 -> [r, intersectBounds b1 b2]
        [0..14] |> List.collect (fun r -> getRegBnd r)
        
        
        



   


    /// generate arbitrary random uint32
    /// cannot do this with System.Random directly because of positive int restriction
    let uiAllRand() =
        let r16() = randomT.Next(0,1 <<< 16) |> uint32
        (r16() <<< 16) + r16()

    /// Returns an unsigned random number generator within a range (uMin,uMax)
    /// Both bounds are inclusive (different from System.Random)
    /// made difficult by the crippled System.Random number range of [0..2^31-2]
    let uiRangeRand (uMin:uint32,uMax:uint32) : (unit -> uint32) =
        let uRand (m : uint64) = randomT.Next(int (uint32 m)) |> uint32
        let range = uint64 uMax - uint64 uMin + 1UL
        let range32 = uint32 range
        match range with
        | _ when range <= 0x7FFFFFFFUL ->  
            fun () -> (randomT.Next(0, int range32)|> uint32) + uMin
        | _ ->
            let m1 = range / 3UL
            let m2 = range - m1
            fun () -> 
                match randomT.Next 3 with
                | 0 -> (uRand m1) + uMin
                | 1 -> (m2-m1 |> uRand) + uMin + (uint32 m1)
                | _ -> (range-m2 |> uRand) + uMin + (uint32 m2)

    /// Returns a signed random number generator within a range (iMin,iMax)
    /// Both bounds are inclusive (different from System.Random)
    /// made difficult by the crippled System.Random number range of [0..2^31-2]
    let iRangeRand(iMin:int32,iMax:int32) : (unit -> int32)  =
        fun () ->
            let uRange = (iMax - iMin) |> uint32
            if iMin = Int32.MinValue && iMax = Int32.MaxValue 
            then int32 (uiAllRand())
            else (uiRangeRand(0u, uRange)() |> int32) + iMin



    /// Selected random 32 bit integers good for exploring corner cases of DP instructions
    /// DP corner cases are 0 and 0x80000000
    /// n determines how tightly numbers focus on corners (+/- n each corner)
    let dpRand (n:int) : (unit -> uint32) =
        let large = 0x80000000u
        let un = n |> uint32
        fun () -> 
            randomChoice(
                [
                    iRangeRand(-n,n) >> uint32
                    uiRangeRand(large - un, large + un)
                ]) ()
       
    /// evaluates RegSpec and returns a compatible random value for register number reg
    /// registers not mentioned in RegSpec get arbitrary unint32 values
    let returnInitRegVal reg spec =
        let minMax = (UInt32.MinValue,UInt32.MaxValue)
        match List.tryFind (fun (r,_) -> r = reg) spec with 
        | None -> uiRangeRand minMax ()
        | Some (_, Lim(umin,umax)) -> uiRangeRand(umin,umax)()
        | Some (_, DPB n) -> (dpRand (int n))()
        | Some (_, Val n) -> n


    /// Create a randomised DPath value (registers + flags)
    /// Register values are constrained by rs limits
    let createDPath (rs: RegSpec) =
        let randTF() = 
            match randomT.Next(2) with 
            | 0 -> false 
            | _ -> true 

        let regs =
            [0..14]
            |> List.map (fun n -> (returnInitRegVal n rs))

        let nz = randomT.Next(3)
        {
            TRegs = regs
            TFlags = 
                {
                    FC=randTF() ; 
                    FN = nz = 2 ; 
                    FV =randTF() ; 
                    FZ = nz = 1
                }
        }





    /// Evaluate a TestCode value to return a list of TestRand test specifications.
    /// Each test spec is a function to generate (randomised) assembler
    /// paired with a RegSpec that specified compatible register value limits
    /// for multiple tests call the returned function multiple times, generating a
    /// new randomised DPath from the RegSpec for each test.
    let rec EVAL (tc:TestCode): TestRand list =
        let combineSpec rs (s, rs') = s, joinRegSpecs rs rs'
        match tc with
        | FORALL lis -> lis |>  List.collect EVAL
        | TR tr -> [tr]
    
    let rec EVALR tr =
        match tr with
        | RAND f -> EVALR (f())
        | SR (s, rs) -> s,rs

    /// Join together assembler strings from r1 and r2.
    /// Exhaustive and randomised parts are correctly joined.
    /// The result TestCode (evaluated by EVAL) leads to a list of
    /// test specs one for each distinct exhaustive test option.
    /// Length increases exponentially as mutiple exhaustive (FORALL)
    /// TestCode parts are joined by ++
    let rec (++) (r1:TestCode) (r2:TestCode) = 
            match r1, r2 with
            | FORALL rl1, r2 -> FORALL (List.map (fun r1 -> r1 ++ r2) rl1)
            | r1, FORALL rl2 -> FORALL (List.map (fun r2 -> r1 ++ r2) rl2)
            | TR a, TR b ->
                TR (RAND (fun () -> 
                            let s1,rs1 = EVALR a
                            let s2,rs2 = EVALR b
                            SR (s1+s2, joinRegSpecs rs1  rs2)))
                

    /// make a list of TestCode into an assembly program, one line each
    let rec ASMLINES lst = 
        match lst with
        | a :: rest -> a ++ S "\n" ++ ASMLINES rest
        | [] -> S ""


 //--------------------------------------------------------------------------------
 //
 //                   CODE FOR ARM DP INSTRUCTION GENERATION
 //
 //--------------------------------------------------------------------------------


 


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

    /// generate a TestRand value from a function that generates random strings
    let toRS f = (fun () -> SR (f(),[])) |> RAND

    /// generate a TC value from  afunction that generates random strings
    let toTC = toRS >> TR

    /// generate an RS value from a TC value (FORALLs are randomly evaluated in the TestRand)
    let rec tcToRS tc = 
        match tc with
        | FORALL lst -> RAND (fun () -> tcToRS lst.[randomT.Next lst.Length])
        | TR rs -> rs

    /// Combine a list of TCs into a single one with a random choice each time it is evaluated
    let RANDTC tcl = tcl |> List.collect EVAL |> randomChoice |> TR

    
    let DPREG = toTC <| fun () -> sprintf "R%d" (dpRegNum())

    let WS = toTC <| fun () ->  [" ";"\t"].[randomT.Next(2)]

    let WSQ = toTC <| fun () -> [" ";"\t";""].[randomT.Next(3)]

    let IMM = toTC <| fun () -> sprintf "#%d" (int (immediate()))

    /// evaluate a TestCode completely to generate a random string
    let rec EvalRTC tc = 
        match tcToRS tc with
        | RAND tr  -> tr() |> TR |> EvalRTC
        | SR (s, rs) -> s,rs
    
    let tcToS = EvalRTC >> fst
    
    let Bound tc rs = 
        let (s, rs') = EvalRTC tc
        s, joinRegSpecs rs rs'
    
    let makeTCBound rs = ("",rs) |> SR |> TR

    let SHIFTAMT = TR ( RAND (fun () ->
         
          SR (  match randomT.Next(20) with
                | n when n < 10 -> 
                    let r = dpRegNum()
                    let reg = sprintf "R%d" r
                    Bound (WS ++ S reg) [r,Lim(0u,32u)]
                | _ -> EvalRTC (WS ++ toTC (fun () -> randomT.Next(0,33) |> sprintf "#%d")))
          ))

    let SHIFT =
        let mapF so =
            let (|SSS|_|) (tc:TestCode) = 
                match tc with 
                | TR (SR (x,_)) -> Some x 
                | _ -> None
            match so with
            | SSS "" -> S ""
            | SSS "RRX" -> WSQ ++ S "," ++ WSQ ++ S "RRX"
            | SSS s -> WS ++ S "," ++ WSQ ++ S s ++ SHIFTAMT
            | _ -> failwithf "Unexpected test description found in makeshift map function"
        shiftOpRRX |> TESTMAP mapF

    /// evaluate a TestCode using default register limits corresponding to DP corner cases
    /// these are over-ridden by any limits in the specification
    let EVALDP dpLim lis =
        let elis = EVAL lis
        let getRegBnds (rs:RegSpec) n =
            match List.tryFind (fun (r,_) -> r=n) rs with
            | None -> (n, DPB (uint32 dpLim))
            | Some x -> x
        elis
        |> List.map (fun tr () -> 
            let (asm,rs) = EVALR tr
            ([0..14] |> List.map (getRegBnds rs) |> createDPath), asm
            )

    let opImm() =  WSQ ++ S "," ++ WSQ ++ S "," ++ WSQ ++ IMM
    let opShift() = WSQ ++ S "," ++ WSQ ++ DPREG ++ SHIFT
    let op2() = WSQ ++ S "," ++ DPREG
    let makeDPOp opc ops = EVALDP 4 <|  opc ++ WS ++ DPREG ++ ops

    let dp3Shifts() = makeDPOp dp3OpC  (op2()  ++ opShift())
    let dp3Imms() = makeDPOp dp3OpC (op2() ++ opImm())
    let dp2Shifts() = makeDPOp dp2OpC (WSQ ++ opShift())
    let dp2Imms() = makeDPOp dp2OpC (opImm())



//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//                                                MEMORY COMMON CODE
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let SEP = WSQ ++ S "," ++ WSQ
    let BRA = S "[" ++ WSQ
    let KET = WSQ ++ S "]" 

    let RG (n:int) = S (sprintf "R%d" n)

    let makeDCD lab (numb:int) = 
                S lab ++ WS ++ S "DCD"  ++ WS ++ (
                    [0..numb-1] 
                    |> List.map (fun _ -> uiAllRand() |> sprintf "%d" |> S  )
                    |> List.reduce (fun a b -> a ++ SEP ++ b))

    let chooseReg (pool: int list) = 
        let chosen = pool.[randomT.Next pool.Length]
        let pool' = List.filter ((<>) chosen) pool
        (chosen, pool')

    let chooseNo13Reg pool =
        let poolNo13 = List.filter ((<>) 13) pool
        let chosen = poolNo13.[randomT.Next poolNo13.Length]
        let pool' = List.filter ((<>) chosen) pool
        (chosen, pool')
    
    let liftChoose chooseFun (chosenLst, pool) =
        let choice, pool' = chooseFun pool
        chosenLst @ [choice], pool'

    let choose2 choosers pool =
                match ([],pool) |> choosers with
                | [a;b], p -> a,b,p
                | _ -> failwith "What - can't happen!"

    
    let choose3 choosers pool =
                match ([],pool) |> choosers with
                | [a;b;c], p -> a,b,c,p
                | _ -> failwith "What - can't happen!"

    let choose4 choosers pool =
            match ([], pool) |> choosers with
            | [a;b;c;d], p -> a,b,c,d,p
            | _ -> failwith "What - can't happen!"

    let choose5 choosers pool =
        match ([], pool) |> choosers with
        | [a;b;c;d;e], p -> a,b,c,d,e,p
        | _ -> failwith "What - can't happen!"

    let choose6 choosers pool =
        match ([], pool) |> choosers with
        | [a;b;c;d;e;f], p -> a,b,c,d,e,f,p
        | _ -> failwith "What - can't happen!"


    let CR = liftChoose chooseReg
    let CRN13 = liftChoose chooseNo13Reg

    let EVALMEM lis =
        let elis = EVAL lis
        let getRegBnds (rs:RegSpec) n =
            match List.tryFind (fun (r,_) -> r=n) rs with
            | None -> (n, Lim(0u,UInt32.MaxValue))
            | Some x -> x
        elis
        |> List.map (fun tr () -> 
            let (s,rs) = EVALR tr
            ([0..14] |> List.map (getRegBnds rs) |> createDPath), s
            )
    

    let makeDCDs() =
        ASMLINES [
            makeDCD "DAT1" 16  
            makeDCD "DAT2" 16
            makeDCD "DAT3" 16 
            makeDCD "DAT4" 16
        ]

    /// Make DCDs and also code that Checksums them.
    /// pool -> list of fre registers from which the 3 registers used by check are chosen.
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
                   
 
    /// this must be the same as for that used by Visual (ensured programmatically by this project) and that used by Visual2 (no guarantee)
    let dataSectionStart = 0x200u


        


    let makeMemImm() = 
        let imms = [ 1;3;-1;-5;8;12;16;20;-8;-12;-16;-20]
        imms.[randomT.Next(imms.Length)]

    let mImm =
        S "#" ++ 
            FORALL [ 
                toTC <|  fun () -> sprintf "0x%x" (makeMemImm()) 
                toTC <| fun () -> sprintf "%d" (makeMemImm()) 
            ]

    let mShiftAmt rx = TR (RAND <|  fun () ->
        let n = randomT.Next(1,4)
        let b = match n with
                | 1 -> 8
                | 2 -> 4
                | _ -> 1
        let r = randomT.Next(10,14)
        Bound (S <| sprintf "#%d" b) [rx, Lim(1u , uint32 n)]
        |> SR)
    

    let inSide r rx = 
        FORALL [
            RG r 
            RG r ++ SEP ++ mImm  
            RG r ++ SEP ++ RG rx
            RG r ++ SEP ++ RG rx ++ SEP ++ FORALL [S "LSL" ; S "LSR"; S "ASR"] ++ WS ++ mShiftAmt rx
        ]

    let memMode r rx = 
        FORALL [  
            BRA ++ inSide r rx ++ KET ++ WSQ ++ ALLSTRINGS [ "" ; "!"]
            BRA ++ RG r ++ KET ++ SEP ++ mImm
        ]
   
    let memSingleTest isLoad = 
        let memOpCodes = ALLSTRINGS ["LDR" ; "STR"]
        let memOpSuffs = ALLSTRINGS ["";"B"]
        let regPool = [0..14]
        let mData, mBase, mExtra, pool' = choose3 (CR >> CR >> CR) regPool

        let memLoadsStores isLoad mData mBase mExtra = 
            let baseOpCode = if isLoad then "LDR" else "STR"
            S baseOpCode ++ ALLSTRINGS [ "" ; "B"] ++ WS ++ RG mData ++ SEP ++ (memMode mBase mExtra) ++
                makeTCBound  [mExtra, Lim(1u,3u)]
        EVALMEM <|
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

    let memMultiTest isLoad =
     
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

        EVALMEM <|
            let regPool = [0..14]
            let mData1,mData2,mBase,pool' = choose3 (CRN13 >> CRN13 >> CR) regPool
            ASMLINES [ 
                memLDMSTM isLoad mBase mData1 mData2 
                makeMemCheckCode pool'
            ]

