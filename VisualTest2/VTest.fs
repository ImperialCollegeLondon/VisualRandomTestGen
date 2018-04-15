namespace VisualTest

/// top-level code demonstrating how to run tests

module VTest =

    open VCommon
    open VLog
    open Visual
    open System.Threading
    open System.IO

    type rType = {
        R0:int;R1:int;R2:int;R3:int;R4:int;R5:int;R6:int;R7:int;
        R8:int;R9:int;R10:int;R11:int;R12:int;R13:int;R14:int
    }

    type MemLocType = {
        Addr: uint32
        Data: uint32
        }

    let rType2List (r:rType)=
        [r.R0;r.R1;r.R2;r.R3;r.R4; r.R5;r.R6;r.R7;
         r.R8;r.R9;r.R10;r.R11;r.R12;r.R13;r.R14]




    


    type DecodedOutput = {
        ORegisters: uint32 list
        OFlags: Flags
        OMemLocs: uint32 list
        }

    let decodeVisOutput (p:Params)  (v:VisOutput) =
        {
            OFlags = v.State.VFlags 
            OMemLocs = v.State.VMemData
            ORegisters = v.Regs |> List.sort |> List.map (fun (o,v) -> uint32 v) 
        }


    

