namespace VisualTest
module VProgram =

    open VCommon
    open VLog
    open Visual
    open VTest
    open System.Threading
    open System.IO

    let srcDir =  __SOURCE_DIRECTORY__

    /// Parameters setting up the testing framework
    /// Standard postlude contains instructions to move CPU state (flags, memory locations) into registers
    /// Simulation reads VisUAL log and returns registers after test assembly code, and also after postlude
    /// Framework code interprets registers after postlude as memory locations and flags
    let defaultParas = {
        Parallel = true              // parallel testing is now supported!
        MaxConcurrentVisualDirs = 10 // should only need the same number as of cores
        Cached = true                // true if results are stored in a cache on disk and reused to speed 
                                     // up future repeat simulations
        VisualPath =  
            srcDir + @"\..\visualapp\visual\"  // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir = 
            srcDir + @"\..\VisualWork\"        // the directory in which both temporary files and the persistent cache file are put
        CacheFileName = 
            srcDir + @"\..\VisualWork\Cache"   // the file name of the global cache
        CacheLimit = 10               // the number of results before adding to global cache
        InitFlags = {FN=false;FZ=false; FC=false;FV=false}
        InitRegs = [0u..10u..140u]          // initial values of registers R0..R14
        MemReadBase = 0x1000u          // locations read from memory (currently 13 consecutive words are read)
        Postlude = ""                 // this is overwritten by code
        Prelude = ""                  // this is overwritten by code
    } 

        


    let vsoToDP (vso:Result<VisOutput,string list>) =
        let regDecode vso = 
            [0..14] 
            |> List.map (fun n -> List.find (fun (R n', x) -> n=n') vso.Regs)
            |> List.map (fun (R n, x) -> uint32 x)
        match vso with
        | Error e -> None
        | Ok v ->
            {
                TRegs = regDecode v
                TFlags = v.State.VFlags
            } |> Some

    let runDPTest (test: unit -> DPath * string) (logRoot:string) (np:int) (n:int) =
        let fName = sprintf "%s%d.txt" logRoot np
        let paras = defaultParas
        let path,asm = test()
        let vso = 
            RunVisualWithFlagsOut {paras with 
                                    InitRegs = path.TRegs
                                    InitFlags = path.TFlags} asm
        let ts = 
            {
                Name = sprintf "%s:%d" logRoot n
                Before = path ; Asm = asm ; After = vsoToDP vso
            }
        File.AppendAllText( paras.WorkFileDir + fName, saveState ts + "\r\n")
                
        

    let runDPTests test np size logRoot =
        [0..size-1]
        |> List.iter (fun i -> runDPTest test logRoot np (np*size+i); printf " %d.%d " np i)
     


    let runTests() =
        0
    
    let runTestsInParallel paras testFns testSize =
        let numThreads = defaultParas.MaxConcurrentVisualDirs
        testFns
        |> List.iter (fun (testFn,logRoot) -> 
            let logName n = sprintf "%s%d.txt" logRoot n
            printfn "\nStarting %s" logRoot
            Seq.init  numThreads (fun n -> async {runDPTests testFn n testSize logRoot})
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore

            List.init  paras.MaxConcurrentVisualDirs (fun n ->
                paras.WorkFileDir + logName n
                |> fun fn -> 
                        let txt = File.ReadAllText fn
                        File.Delete fn
                        txt)
            |> String.concat ""
            |> fun txt -> File.WriteAllText(paras.WorkFileDir + logRoot + "All.txt",txt)
            printfn "\nFinished %s.\n\n" logRoot
            )


    [<EntryPoint>]
    let main _ = 
        let tests = [
            VRandom.dp3Imms, "dp3Imm";
            VRandom.dp2Imms, "dp2Imm";
            VRandom.dp3Shifts,"dp3Shifts";
            VRandom.dp2Shifts, "dp2Shifts"
            ]
        initCaches defaultParas
        printfn "Caches initialised"
        runTestsInParallel defaultParas tests 200
        printfn "Tests completed"
        finaliseCaches defaultParas
        printfn "Caches finalised"
        System.Console.ReadKey() |> ignore                
        0 // return an integer exit code - 0 if all tests pass
