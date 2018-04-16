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

    let dpRand (n:int) =
        let large = 0x80000000u
        let un = n |> uint32
        let u = uniformRan(
                    [
                      iRangeRand(-n,n) >> uint32
                      uiRangeRand(large - un, large + un)
                    ])
        fun () -> u()()
        

    let dp3OpC = 
        uniformRan [ 
            "ADD";"ADC";"SUB";"SBC";"RSB";"RSC"; 
            "AND";"EOR";"BIC";"ORR" ; "ADDS"; 
            "ADCS"; "SBCS"; "RSBS"; "RSCS" ; "ANDS"; "EORS" ; "BICS"
        ]
    
    let dp2OpC =
        uniformRan [ "MOV"; "MVN"; "TST"; "TEQ"; "CMN"; "MOVS"; "MVNS" ]
    
    let shiftOp = uniformRan [ "" ; "ASR" ; "LSR" ; "LSL" ; "ROR" ]

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

        