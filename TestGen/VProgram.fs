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
        MaxConcurrentVisualDirs = 6 // should only need the same number as of cores
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
        MemReadBase = VRandom.dataSectionStart          // locations read from memory (currently 13 consecutive words are read)
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

    let runDPTest (tList: (int * (DPath * string)) list) (logRoot:string) (np:int) (totalNum:int) (numThreads:int)=
        let fName = sprintf "%s%d.txt" logRoot np
        let paras = defaultParas
        let numTests = tList.Length

        let mutable watch: System.Diagnostics.Stopwatch option = None
        tList
        |> List.sortDescending
        |> List.iter (fun (i, (path,asm)) ->
            if i = totalNum - 5 && np = numThreads-1 then
                watch <- Some <| System.Diagnostics.Stopwatch.StartNew()
            match  watch with 
            | Some w when np = numThreads - 1 && i = totalNum - 15 ->
                w.Stop()
                let msTime = w.Elapsed.TotalMilliseconds / (10.0 * float numThreads)
                printfn "\n\nTime per test: %.1fms. Total time for this test set: %.1fs\n" 
                    msTime (msTime * float numThreads * float numTests / 1000.0)
            | _ -> ()

            let vso = 
                RunVisualWithFlagsOut {
                    paras with 
                        InitRegs = path.TRegs
                        InitFlags = path.TFlags
                } asm
            let ts = {
                    Name = sprintf "%s:%d" logRoot (numThreads*i + np)
                    Before = path ; Asm = asm ; After = vsoToDP vso
                }
            File.AppendAllText( paras.WorkFileDir + fName, saveState ts + "\r\n")
            printf "%d.%d " np i
           )
                
     
    
    let runTestsInParallel paras testFns testSize =
        let numThreads = defaultParas.MaxConcurrentVisualDirs
        testFns
        |> List.iter ( fun ((testFn: unit -> (DPath * string) list) , logRoot) -> 
                let logName n = sprintf "%s%d.txt" logRoot n
                printfn "\nStarting %s" logRoot
                let tNum = testFn().Length
                let totalNum = max tNum testSize
                let tests = 
                    seq { while true do yield! testFn()}
                    |> Seq.take totalNum
                    |> Seq.toList
                    |> List.indexed
                    |> List.groupBy (fun (i,_) -> (float i * float numThreads) / float totalNum |> int)
                tests
                |> List.map  (fun (i, tLis) -> 
                        async {runDPTest tLis logRoot i totalNum numThreads})
                |> Async.Parallel
                |> Async.RunSynchronously
                |> ignore

                List.init  paras.MaxConcurrentVisualDirs (fun n ->
                    paras.WorkFileDir + logName n
                    |> fun fn -> 
                            let txt = File.ReadAllText fn
                            File.Delete fn
                            txt
                )
                |> String.concat ""
                |> fun txt -> File.WriteAllText(paras.WorkFileDir + logRoot + "All.txt",txt) |> ignore
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
        let memTests = [
            VRandom.memLDRX, "MemLoads"
            ]
        initCaches defaultParas
        printfn "Caches initialised"
        //VRandom.memLDRX() |> List.iter (fun (a,b) -> printfn "%s\t%A" (a()) b)
        runTestsInParallel defaultParas memTests 40
        printfn "Tests completed"
        finaliseCaches defaultParas
        printfn "Caches finalised"
        System.Console.ReadKey() |> ignore                
        0 // return an integer exit code - 0 if all tests pass
