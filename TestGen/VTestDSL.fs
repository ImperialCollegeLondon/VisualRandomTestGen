namespace VisualTest

open System


module VTestDSL =

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
    /// recursive definition allows RegSpec register constraints to be linked to strings
    /// and random generation functions
    type TestCode =
        | FORALL of TestCode list
        | BOUNDED of (TestCode * RegSpec)
        | RAND of (unit -> TestCode)
        | S of string

  

    /// construct TestCode for exhaustive tests, one for each of the strings
    /// total number of tests will be product of all joined exhaustive tests
    /// Compare this with RANDSTRINGS
    let ALLSTRINGS lst = FORALL (lst |> List.map S)

    /// construct TestCode for a randomised test: every test containing this will be given a new
    /// randomly generated string from the list. compare this with ALLSTRINGS
    let RANDSTRINGS (lst: string list) = 
        fun () -> S lst.[randomT.Next lst.Length]
        |> RAND 

    /// combine a lst of TestCodes as a random test (not exhaustive)
    /// this contrasts with FORALL which will ensure exhaustive testing
    let RANDTC = FORALL >> (fun x -> RAND (fun () -> x))
 
        

    /// map over a FORALL list
    /// NB this does not operate on nested forall lists
    let TESTMAP map (tc:TestCode) =
        match tc with
        | FORALL lis -> List.map map lis |> FORALL
        | _ -> failwithf "Unexpected Test case in testMap %A" tc


    /// Return minB..maxB RegBounds for multiple registers
    let makeBounds rl minB maxB =
        rl |> List.map (fun n -> n, Lim(minB,maxB))

    /// shorthand alias for the EExtensions regex matcher AP
    let (|MATCH|_|) re s = String.regexMatch re s 

    /// AP to parse integers
    let (|PARSEINT|_|) s = 
        match Int32.TryParse s with 
        | true , n -> Some n 
        | false , _ -> None
    
    /// AP to return reg int from reg string: R3 -> 3
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
        | RAND _ -> []
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
       


    /// Create a randomised DPath value (registers + flags)
    /// Register values are initialised by regInits function created in genTests
    let createDPath (flagInits: unit -> Flags) (regInits: (int -> uint32)) =
        {
            TRegs =
                [0..14]
                |> List.map regInits
            TFlags = flagInits()
        }

    /// Evaluate a TestCode value to return a list of  test specifications.
    /// Each test spec is a function to generate (randomised) assembler
    /// paired with a RegSpec that specified compatible register value limits
    /// for multiple tests call the returned function multiple times, generating a
    /// new randomised DPath from the RegSpec for each test.
    let rec EVAL (tc:TestCode): (unit -> string * RegSpec) list =
        let combineSpec rs (s, rs') = s, joinRegSpecs rs rs'
        match tc with
        | FORALL lis -> 
            lis 
            |>  List.collect EVAL
        | S s -> 
            [ 
                fun _ -> s , [] 
            ]
        | RAND f -> 
            [
                fun _ -> EVAL (f()) |> randomChoice |> (fun f -> f())
            ]
        | BOUNDED (tc,rs) -> 
            EVAL tc 
            |> List.map ( fun f _ -> 
                match f() with 
                | s , rs' -> s, joinRegSpecs rs rs'
            )

    /// evaluate a TestCode completely to generate a random string paired with a RegSpec
    let rec EVALS tc = 
        match EVAL tc with
        | h :: t as lst -> lst |> randomChoice |> fun f ->f()
        | [] -> failwithf "What? %A is empty when evaluated!" tc

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
            | S a, S b -> a + b |> S
            | a, BOUNDED (b,rs)
            | BOUNDED (a,rs) , b -> BOUNDED (a ++ b,rs)
            | RAND f , b -> (fun _ -> f() ++ b) |> RAND
            | a, RAND f -> (fun _ -> a ++ f()) |> RAND
                

    /// make a list of TestCode into an assembly program, one line each
    let rec ASMLINES lst = 
        match lst with
        | a :: rest -> a ++ S "\n" ++ ASMLINES rest
        | [] -> S ""


    let genAnyValue = uiRangeRand (UInt32.MinValue,UInt32.MaxValue)

    /// Evaluate a TestCode to make a list of functions
    /// each specifying randomised tests.
    /// The list elements represent exhaustive tests.
    /// Also return a DPath dataPath which respects all 
    /// initial register value bounds in
    /// tc but otherwise has arbitrary randomly generated
    /// registers and flags
    let genTests (flagsInit: unit->Flags) (defaultBounds: unit -> uint32) (tc:TestCode) : (unit -> DPath*string) list =
        /// evaluates RegSpec and returns a compatible random value for register number reg
        /// registers not mentioned in RegSpec get arbitrary unint32 values

        let genInitRegVal bound =
            match bound with
            | Lim(umin,umax) -> uiRangeRand(umin,umax)
            | DPB n -> (dpRand (int n))
            | Val n -> fun () -> n

        let testGenLst = EVAL tc
        let getRegBnd (rs:RegSpec) n =
            match List.tryFind (fun (r,_) -> r=n) rs with
            | None -> defaultBounds()
            | Some (_,x) -> genInitRegVal x ()
        testGenLst
        |> List.map (fun testGenerator _ -> 
            let (asm,regSpec) = testGenerator ()
            (flagsInit, getRegBnd regSpec)
            ||> createDPath
            |> fun dataPath -> dataPath, asm )

    let randomFlags() =
        let randTF() = 
            match randomT.Next(2) with 
            | 0 -> false 
            | _ -> true 
        let nz = randomT.Next(3)
        {
            FC=randTF() ; 
            FN = nz = 2 ; 
            FV =randTF() ; 
            FZ = nz = 1
        }


    let GENTESTS = 
        genTests randomFlags (fun _ -> uiAllRand())

    let GENTESTSZEROINIT = 
        genTests (fun _ -> {FN=false;FZ=false;FC=false;FV=false}) (fun _ -> 0u)

        
    /// TestCode for random whitespace separator
    let WS = RAND <|  fun () -> S  [" ";"\t"].[randomT.Next(2)]

    /// TestCode for random OPTIONAL whitespace separator
    let WSQ = RAND <| fun () -> S [" ";"\t";""].[randomT.Next(3)]

