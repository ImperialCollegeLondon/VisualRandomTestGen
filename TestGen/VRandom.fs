namespace VisualTest

open System


module VRandom =

    open System
    open VCommon
    open EEExtensions

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

    /// constructor for randomly generated assembler strings
    /// recursive definition allows register constraints to be linked to strings
    type TestCode =
        | BOUNDED of TestCode * RegSpec
        | RAND of (Unit->TestCode)
        | S of string
        | FORALL of TestCode list

    /// construct TestCode for exhaustive tests, one for each of the strings
    /// total number of tests will be product of all joined exhaustive tests
    /// Compare this with RANDSTRINGS
    let ALLSTRINGS lst = FORALL (lst |> List.map S)

    /// construct TestCode for a randomised test: every test containing this will be given a new
    /// randomly generated string from the list. compare this with ALLSTRINGS
    let RANDSTRINGS (lst: string list) = RAND (fun () -> S lst.[randomT.Next(lst.Length)])

    let TESTMAP map tc =
        match tc with
        | FORALL lis -> List.map map lis |> FORALL
        | _ -> failwithf "Unexpected Test case in testMap %A" tc

    /// op-codes for 3 operand instructions to test


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
        | S "PC" -> Some 15
        | S "LR" -> Some 14
        | S "SP" -> Some 13
        | S (MATCH "R([0-9])+" (PARSEINT n))  -> Some n
        | _ -> None
    
    /// Extract a list of register numbers from a TestCode
    /// e.g. FORALL [S "R10" ; S "R3"] -> [3 ; 10]
    /// Also works on single registers (S "R3")
    /// Will correctly recurse to inner FORALL & BOUNDED cases
    /// Will ignore registers inside other TestCode cases
    /// The reg number list is sorted in ascending order
    let rec fromRegTC (regLst: TestCode) = 
        match regLst with
        | REGNUM n -> [n]
        | FORALL rLst -> 
            rLst 
            |> List.collect (function | REGNUM n -> [n] | _ -> [])
        | BOUNDED(tc,_) -> fromRegTC tc
        | RAND _ -> []
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

    /// Evaluate a TestCode value to return a list of test specifications.
    /// Each test spec is a function to generate (randomised) assembler
    /// paired with a RegSpec that specified compatible register value limits
    /// for multiple tests call the returned function multiple times, generating a
    /// new randomised DPath from the RegSpec for each test.
    let rec EVAL (tc:TestCode): ((Unit->string) * RegSpec) list =
        let combineSpec rs (s, rs') = s, joinRegSpecs rs rs'
        match tc with
        | FORALL lis -> lis |>  List.collect EVAL
        | BOUNDED (tc, rs) -> tc |> EVAL |> List.map (combineSpec rs)
        | RAND f -> f () |> EVAL 
        | S s -> [(fun () -> s) , []]

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
            | BOUNDED (r1, bnd), r2
            | r1, BOUNDED (r2,bnd) -> BOUNDED (r1 ++ r2, bnd)
            | RAND r1, r2 -> RAND (fun () -> r1() ++ r2)
            | r1, RAND r2 -> RAND (fun () -> r1 ++ r2())
            | S r1, S r2 -> S (r1 + r2)



 //--------------------------------------------------------------------------------
 //
 //                   CODE FOR ARM INSTRUCTION GENERATION
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

    let DPREG = RAND (fun () -> S <| sprintf "R%d" (dpRegNum()))

    let WS = RAND (fun () ->  S [" ";"\t"].[randomT.Next(2)])

    let WSQ = RAND (fun () -> S [" ";"\t";""].[randomT.Next(3)])

    let IMM = RAND (fun () -> S <| sprintf "#%d" (int (immediate())))



      
    

    let SHIFTAMT =
        RAND (fun () ->
                match randomT.Next(20) with
                | n when n < 10 -> 
                    let r = dpRegNum()
                    let reg = sprintf "R%d" r
                    BOUNDED( WS ++ S reg, [r,Lim(0u,32u)])
                | _ -> WS ++ RAND (fun () -> (randomT.Next(0,33) |> sprintf "#%d" |> S))
             )


    let SHIFT =
        let mapF so =
            match so with
            | S "" -> S ""
            | S "RRX" -> WSQ ++ S "," ++ WSQ ++ S "RRX"
            | S s -> WSQ ++ S "," ++ WSQ ++ S s ++ SHIFTAMT
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
        |> List.map (fun (fs,rs) -> 
            ([0..14] |> List.map (getRegBnds rs) |> createDPath), (fs())
            )

    let opImm() =  WSQ ++ S "," ++ WSQ ++ S "," ++ WSQ ++ IMM
    let opShift() = WSQ ++ S "," ++ WSQ ++ DPREG ++ SHIFT
    let opWithDest opc = opc ++ WS ++ DPREG

    let dp3Shifts() = EVALDP 4 <| opWithDest dp3OpC ++ WSQ ++ S "," ++ DPREG ++ opShift()
    let dp3Imms() = EVALDP 4 <| opWithDest dp3OpC ++ WSQ ++ S "," ++ DPREG ++ opImm()
    let dp2Shifts() = EVALDP 4 <| opWithDest dp2OpC ++ WSQ ++ opShift()
    let dp2Imms() = EVALDP 4 <| opWithDest dp2OpC ++ opImm()



//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//                            MEMORY SINGLE REGISTER LOAD/STORE INSTRUCTIONS
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let makeDCD lab (numb:int) = 
        [lab ; "DCD" ] @ (
            [0..numb] 
            |> List.map ( fun _ -> sprintf "%d" (uiAllRand())
                        )
        )
    /// this must be the same as for that used by Visual (ensured programmatically by this project) and that used by Visual2 (no guarantee)
    let dataSectionStart = 0x100

    let memOpCodes = ALLSTRINGS ["LDR" ; "STR"]
    let memOpSuffs = ALLSTRINGS ["";"B"]

    let SEP = WSQ ++ S "," ++ WSQ
    let BRA = S "[" ++ SEP
    let KET = SEP ++ S "]"
 

    let RG (n:int) = S (sprintf "R%d" n)

    let makeMemImm() = 
        let imms = [ 1;3;-1;-5;8;12;16;20;-8;-12;-16;-20]
        imms.[randomT.Next(imms.Length)]

    let MIMM = 
        S "#" ++ 
            FORALL [ 
                RAND <| fun () -> S <| sprintf "0x%x" (makeMemImm()) 
                RAND <| fun () -> S <| sprintf "%d" (makeMemImm()) 
            ]

    let inSide r offset rx = 
        FORALL [
            RG r 
            RG r ++ SEP ++ MIMM  
            RG r ++ SEP ++ RG rx
            RG r ++ SEP ++ RG rx ++ SEP ++ FORALL [S "LSL" ; S "LSR"; S "ASR"]
        ]

    let memMode r offset rx = 
        FORALL [ 
            BRA ++ inSide r offset rx ++ KET  
            BRA ++ inSide r offset rx ++ KET ++ WSQ ++ S "!"
            BRA ++ RG r ++ KET ++ SEP ++ MIMM
        ]
   
    let memLoads mData mBase offset mExtra = 
        S "LDR" ++ ALLSTRINGS [ "" ; "B"] ++ WS ++ RG mData ++ SEP ++ (memMode mBase offset mExtra)
        |> fun tc -> BOUNDED (tc, [])


        