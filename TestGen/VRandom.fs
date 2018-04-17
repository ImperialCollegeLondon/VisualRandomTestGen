namespace VisualTest


module VRandom =

    open System
    open VCommon
    open System.Threading

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

    type RegBound = Lim of string * uint32 * uint32 | Val of string * uint32 | DPB of string

    type TestCode = 
        | FORALL of TestCode list 
        | BOUNDED of TestCode * RegBound list
        | RAND of (Unit->string)
        | S of string

    let addBounds rbl (s, rbl') = s, (rbl @ rbl')

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


    let returnInitRegVal reg rbl =
        let minMax = (false, UInt32.MinValue,UInt32.MaxValue)
        let resolve (isDP, umin, umax) = 
            function | Lim(r,a,b) when r = reg -> isDP, max umin a, min umax b 
                     | Val(r,n) when r = reg && umin <= n && umax >= n  -> (isDP, n, n)
                     | Val(r,n) -> failwithf "Inconsistent register value (%d) for %s" n reg
                     | DPB r -> true, umin, umax
                     | _ -> (isDP, umin,umax)

        let (isDP, umin,umax) = List.fold resolve minMax rbl
        match isDP with
        | false -> uiRangeRand(umin,umax)()
        | true when umax < 33u -> uiRangeRand(umin,umax)()
        | true -> (dpRand 4)()

    let createDPath (rbl: RegBound list) =
        let randTF() = 
            match randomT.Next(2) with 
            | 0 -> false 
            | _ -> true 

        let regs =
            [0..14]
            |> List.map (fun n -> sprintf "R%d" n)
            |> List.map (fun reg -> (returnInitRegVal reg rbl))

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


    let rec Eval (tc:TestCode): ((Unit->string) * RegBound list) list =
        match tc with
        | FORALL lis -> lis |> List.collect Eval 
        | BOUNDED (tc, rbl) -> tc |> Eval |> List.map (addBounds rbl)
        | RAND f -> [f , []]
        | S s -> [(fun () -> s) , []]

    let joinTests (tcl: TestCode list) = 
        let join1 st lis = 
            List.allPairs st lis 
            |> List.map (fun ((s1, rbl1),(s2,rbl2)) -> (fun () -> s1()+s2()), rbl1 @ rbl2)
        tcl 
        |> List.map Eval 
        |> List.fold join1 [(fun ()->""),[]]
        |> List.map (fun (fs,rbl) -> createDPath rbl, fs())




 //--------------------------------------------------------------------------------
 //
 //                   CODE FOR ARM INSTRUCTION GENERATION
 //
 //--------------------------------------------------------------------------------

        
    /// op-codes for 3 operand instructions to test

    let dp3OpC = 
        uniformRan [ 
            "ADD";"ADC";"SUB";"SBC";"RSB";"RSC"; 
            "AND";"EOR";"BIC";"ORR" ; "ADDS"; 
            "ADCS"; "SBCS"; "RSBS"; "RSCS" ; "ANDS"; "EORS" ; "BICS"
        ]

    /// op-codes for 2 operand instructions to test    
    let dp2OpC =
        uniformRan [ "MOV"; "MVN"; "TST"; "TEQ"; "CMN"; "MOVS"; "MVNS" ]

    /// shift operands (without RRX) to test  
    let shiftOp = uniformRan [ "" ; "ASR" ; "LSR" ; "LSL" ; "ROR" ]


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
    let dReg() = sprintf "R%d" (match randomT.Next(14) with | 13 -> 14 | n -> n)

    let WS = uniformRan [" ";"\t"]

    let WSQ = uniformRan [" ";"\t";""]

    let IMM() = (sprintf "#%d" (immediate() |> int))

    let shiftAmt() =
        match randomT.Next(20) with
        | 0 -> WS() + dReg() 
        | _ -> WS() + (sprintf "#%d" <| randomT.Next(0,33))


    let cont x = fun () -> x

    let makeShift() =
        match shiftOp() with
        | "" -> ""
        | "RRX" -> WSQ() + "," + WSQ() + "RRX"
        | s -> WSQ() + "," + WSQ() + s + shiftAmt()


 
    let eval lis = lis |> List.map (fun x -> x()) |> String.concat ""

    let makeRTest syntaxLst = 
        let randTF() = 
            match randomT.Next(2) with 
            | 0 -> false 
            | 1 -> true 
            | _ -> failwithf "What? Cannot happen!"
        /// 0 -> NZ=00 ; 1 -> NZ=01 ; 2 -> NZ=10
        let ran = dpRand 4
        let nz = randomT.Next(3)
        {
            TRegs = List.init 15 (fun _ -> ran()) 
            TFlags = {FC=randTF() ; FN = nz = 2 ; FV =randTF() ; FZ = nz = 1}
        }, (syntaxLst |> eval)


    let dp3Shifts() =  makeRTest [ dp3OpC ; WS ; dReg ; WSQ; cont ","; dReg ; WSQ; cont "," ; WSQ; dReg ; makeShift]
    let dp3Imms() = makeRTest [dp3OpC ; WS; dReg ; WSQ; cont ","; dReg ; WSQ ; cont "," ; WSQ; IMM]
    let dp2Shifts() = makeRTest [ dp2OpC ; WS ; dReg ; WSQ; cont "," ; WSQ; dReg ; makeShift]
    let dp2Imms() = makeRTest [dp2OpC ; WS; dReg ; WSQ; cont ","; dReg ; WSQ ; cont "," ; WSQ; IMM]

        