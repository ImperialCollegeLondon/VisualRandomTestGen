namespace VisualTest
module VProgram =

    open VCommon
    open VLog
    open Visual
    open VTest
    open System.Threading
    open System.IO
    open VTestSpec

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
        MemReadBase = VTestSpec.dataSectionStart          // locations read from memory (currently 13 consecutive words are read)
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

    let runDPTest (tList: (int * (DPath * string)) list) (logRoot:string) (np:int) (numThreads:int)=
        let fName = sprintf "%s%d.txt" logRoot np
        let paras = defaultParas
        let numTests = tList.Length

        tList
        |> List.sortDescending
        |> List.sumBy (fun (i, (path,asm)) ->
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
            if ts.After = None then (1.0 / float tList.Length) else 0.0
           )


                
     
    
    let runTestsInParallel paras testFns  =
        let numThreads = defaultParas.MaxConcurrentVisualDirs
        testFns
        |> List.iter ( fun ((testFns: (unit -> DPath * string) list) , logRoot, testSize) -> 
                let logName n = sprintf "%s%d.txt" logRoot n
                printfn "\nStarting %s" logRoot
                let tNum = testFns.Length
                let totalNum = max tNum testSize
                printfn "Runnng %d tests, %.1f random tests per exhaustively tested item" totalNum (float totalNum/ float tNum)
                let watch: System.Diagnostics.Stopwatch =
                    System.Diagnostics.Stopwatch.StartNew()


                let tests = 
                    seq { while true do yield! (List.map (fun f -> f()) testFns)}
                    |> Seq.take totalNum
                    |> Seq.toList
                    |> List.indexed
                    |> List.groupBy (fun (i,_) -> (float i * float numThreads) / float totalNum |> int)
                tests
                |> List.map  (fun (i, tLis) -> async {
                    let errorFrac = runDPTest tLis logRoot i numThreads
                    return errorFrac
                })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.average
                |> (fun frac -> printfn "\n\n*** %.1f%% VisUAL ERRORs\n" (frac*100.0))

                List.init  paras.MaxConcurrentVisualDirs (fun n ->
                    paras.WorkFileDir + logName n
                    |> fun fn -> 
                            match File.Exists fn with
                            | true ->
                                let txt = File.ReadAllText fn
                                File.Delete fn
                                txt
                            | false -> ""
                )
                |> String.concat ""
                |> fun txt -> File.WriteAllText(paras.WorkFileDir + logRoot + "All.txt",txt) |> ignore
                let time = watch.ElapsedMilliseconds
                printfn "Finished %s in time %.1f s, %d ms per test." logRoot (float time / 1000.0) (int time/ totalNum)) 
            

            
    [<EntryPoint>]
    let main _ = 
        let tests = [
            VTestSpec.dp3Imms, "dp3Imm", 40
            VTestSpec.dp2Imms, "dp2Imm", 40
            VTestSpec.dp3Shifts,"dp3Shifts", 150
            VTestSpec.dp2Shifts, "dp2Shifts", 100
            ]
        let singleMemTests = [
            VTestSpec.memSingleTest true, "MemLoads", 80
            VTestSpec.memSingleTest false, "MemStores", 80
            ]
        let multMemTests = [
            VTestSpec.memMultiTest true, "MultMemLoads", 80
            VTestSpec.memMultiTest false, "MultMemStores", 80
            ]
        let branchTests = [
            //VTestSpec.condBranchTests, "ConditionalBranches", 300
            VTestSpec.computedBranchTests, "ComputedBranches", 0
            VTestSpec.miscTests, "MiscInstr", 0
            ]
        initCaches defaultParas
        printfn "Caches initialised"
        [
            //tests
            //singleMemTests
            //multMemTests
            branchTests
        ] 
        |> List.iter (runTestsInParallel defaultParas)
        printfn "Tests completed"
        finaliseCaches defaultParas
        printfn "Caches finalised"
        System.Console.ReadKey() |> ignore                
        0 // return an integer exit code - 0 if all tests pass
