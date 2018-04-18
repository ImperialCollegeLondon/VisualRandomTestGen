namespace VisualTest


module VRandom =

    open System
    open VCommon
    open System.Threading


    type RegBound = 
        | Lim of uint32 * uint32 /// range
        | Val of uint32 /// single value
        | DPB of uint32 /// +/- n from 0 or 0x80000000
    
    type RegSpec = (int * RegBound) list

    type TestCode =
        | BOUNDED of TestCode * RegSpec
        | RAND of (Unit->TestCode)
        | S of string
        | FORALL of TestCode list


 
    let rec addBnds b1 b2 =
        match b1,b2 with
        | Lim(x1,y1), Lim(x2,y2) -> Lim(max x1 x2, min y1 y2)
        | Val x, b -> addBnds (Lim(x,x)) b
        | DPB x, DPB y -> DPB (min x y)
        | DPB n, Lim(x2,y2) -> failwithf "Can't combine DP and Lim register bounds: %A, %A" b1 b2
        | b1, b2 -> addBnds b2 b1

    let addSpecs (s1:RegSpec) (s2:RegSpec) =
        let grb r (s:RegSpec) = 
            List.tryFind (fun (r',_) -> r = r') s
            |> Option.map (fun (r,b) -> b)
        let getRegBnd r =
            match grb r s1, grb r s2 with
            | None, None -> []
            | None, Some b1
            | Some b1, None -> [(r,b1)]
            | Some b1, Some b2 -> [r, addBnds b1 b2]
        [0..14] |> List.collect (fun r -> getRegBnd r)
        
        
        


    let randomT = Random()

   

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

        // shuffle an array (in-place)
    let shuffle a =
        Array.iteri (fun i _ -> swap a i (randomT.Next(i, Array.length a))) a
    
    let uniformRan (lis:'a list) =
        let items = lis |> List.toArray
        let mutable randLst = items |> Array.toList
        fun () ->
            match randLst with
            | h :: rst -> randLst <- rst ; h
            | [] -> 
                shuffle items
                randLst <- items.[1..] |> Array.toList
                items.[0] 
          
    let xRan (lis: 'a list) =
        lis.[randomT.Next lis.Length]

    let distributionRan (lis: (int*'a) list) =
        let items = 
            lis |> List.collect (fun (i,f) -> List.init i (fun _ -> f))
        uniformRan items
    
    let uiAllRand() =
        let r16() = randomT.Next(0,1 <<< 16) |> uint32
        (r16() <<< 16) + r16()

    /// Returns an unsigned random number generator within a range (uMin,uMax)
    /// Both bounds are inclusive (different from System.Random)
    /// made difficult by the crippled System.Random number range of [0..2^31-2]
    let uiRangeRand (uMin:uint32,uMax:uint32) =
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
    let iRangeRand(iMin:int32,iMax:int32)  =
        fun () ->
            let uRange = (iMax - iMin) |> uint32
            if iMin = Int32.MinValue && iMax = Int32.MaxValue 
            then int32 (uiAllRand())
            else (uiRangeRand(0u, uRange)() |> int32) + iMin



    /// Selected random 32 bit integers good for exploring corner cases of DP instructions
    /// n determines how tightly numbers focus on corners (+/- n each corner)
    let dpRand (n:int) =
        let large = 0x80000000u
        let un = n |> uint32
        let u = uniformRan(
                    [
                      iRangeRand(-n,n) >> uint32
                      uiRangeRand(large - un, large + un)
                    ])
        fun () -> u()()

    let returnInitRegVal reg spec =
        let minMax = (UInt32.MinValue,UInt32.MaxValue)
        match List.tryFind (fun (r,_) -> r = reg) spec with 
        | None -> uiRangeRand minMax ()
        | Some (_, Lim(umin,umax)) -> uiRangeRand(umin,umax)()
        | Some (_, DPB n) -> (dpRand (int n))()
        | Some (_, Val n) -> n



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
                    FZ = nz = 1}
        }


    let rec eval (tc:TestCode): ((Unit->string) * RegSpec) list =
        let combineSpec rs (s, rs') = s, addSpecs rs rs'
        match tc with
        | FORALL lis -> lis |>  List.collect eval
        | BOUNDED (tc, rs) -> tc |> eval |> List.map (combineSpec rs)
        | RAND f -> f () |> eval 
        | S s -> [(fun () -> s) , []]
        
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


    let ALLSTRINGS lst = FORALL (lst |> List.map S)
 
    let RANDSTR (lst: string list) = RAND (fun () -> S lst.[randomT.Next(lst.Length)])
 

    let testMap map tc =
        match tc with
        | FORALL lis -> List.map map lis |> FORALL
        | _ -> failwithf "Unexpected Test case in testMap %A" tc

    /// op-codes for 3 operand instructions to test

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
    let dRegNum() = match randomT.Next(14) with | 13 -> 14 | n -> n
    let dReg num = RAND (fun () -> S <| sprintf "R%d" num)

    let DREG = RAND (fun () -> S <| sprintf "R%d" (dRegNum()))

    let WS = RAND (fun () ->  S [" ";"\t"].[randomT.Next(2)])

    let WSQ = RAND (fun () -> S [" ";"\t";""].[randomT.Next(3)])

    let IMM = RAND (fun () -> S <| sprintf "#%d" (int (immediate())))



      
    

    let SHIFTAMT =
        RAND (fun () ->
                match randomT.Next(20) with
                | n when n < 10 -> 
                    let r = dRegNum()
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
        shiftOpRRX |> testMap mapF

    /// evaluate a TestCode using default register limits corresponding to DP corner cases
    /// these are over-ridden by any limits in the specification
    let evalDP dpLim lis =
        let elis = eval lis
        let getRegBnds (rs:RegSpec) n =
            match List.tryFind (fun (r,_) -> r=n) rs with
            | None -> (n, DPB (uint32 dpLim))
            | Some x -> x
        elis
        |> List.map (fun (fs,rs) -> 
            ([0..14] |> List.map (getRegBnds rs) |> createDPath), (fs())
            )


    let dp3Shifts() = evalDP 4 <| dp3OpC ++ WS ++ DREG ++ WSQ ++ S "," ++ DREG ++ WSQ ++ S "," ++ WSQ ++ DREG ++ SHIFT
    let dp3Imms() = evalDP 4 <| dp3OpC ++ WS ++ DREG ++ WSQ ++ S "," ++ DREG ++ WSQ ++ S "," ++ WSQ ++ IMM
    let dp2Shifts() = evalDP 4 <| dp2OpC ++ WS ++ DREG ++ WSQ ++ S "," ++ WSQ ++ DREG ++ SHIFT
    let dp2Imms() = evalDP 4 <| dp2OpC ++ WS++ DREG ++ WSQ ++ S ","  ++ DREG ++ WSQ ++ S "," ++ WSQ ++ IMM



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
    /// this must be the same as for that used by Visual (ensured programmatically) and that used by Visual2 (no guarantee)
    let dataSectionStart = 0x200
    let memOpCodes = ALLSTRINGS ["LDR" ; "STR"]
    let memOpSuffs = ALLSTRINGS ["";"B"]

    let SEP = WSQ ++ S "," ++ WSQ
    let BRA = S "[" ++ SEP
    let KET = SEP ++ S "]"
 

    let RG (n:int) = S (sprintf "R%d" n)

    let makeMemImm() = 
        let imms = [ 1;3;-1;-5;8;12;16;20;-8;-12;-16;-20]
        imms.[randomT.Next(imms.Length)]

    let MIMM = S "#" ++ 
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

    let MMODE r offset rx = 
        FORALL [ 
            BRA ++ inSide r offset rx ++ KET  
            BRA ++ inSide r offset rx ++ KET ++ WSQ ++ S "!"
            BRA ++ RG r ++ KET ++ SEP ++ MIMM
        ]
   
    let memLoads mData mBase offset mExtra = eval <| S "LDR" ++ ALLSTRINGS [ "" ; "B"] ++ WS ++ RG mData ++ SEP ++ (MMODE mBase offset mExtra)


        