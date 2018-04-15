# VisualRandomTestGen

Generate random test sets from VisUAL simulator for VisUAL2 simulator testing

This project runs under F#/.Net core and drives the VisUAL Java executable, generating simulation outputs for a large set of randomly generated assembly inputs. The typical use case is for single line inputs (see VRandom.fs for generating functions).

### Getting Started

These instructions work for Windows. In theory it should work on other systems but there may be issues with the use of Semaphores, or other incompatibilities.

* Unpack the [VisUAL](https://salmanarif.bitbucket.io/visual/) binaries so that Visual.exe can be found as `./Visualapp/Visual.exe`. 
* The F# .Net core project `./VisualTest2/VisualTest.fsproj` can be reconstituted from a clean .Net Core command line application and the `.fsproj` and `*.fs` files.
* When run the application will generate `./VisualWork/*.txt` files containing random test data: Visual inputs and outputs for given assembly lines. See `vprogram.fs` for top-level file generation and `vrandom.fs` for details of the RNG-based assembler generation.
* Generated files can be put under the `./test-data` directory of Visual2 and will be automatically run.


### More Information

The test framework on which this code is based in documented in ./Visualtest2/Readme.md. However, not all of the framework is used. In fact the whole cache system is not needed and could usefully be removed, see issues.

### Directories

* `./VisualWork/*` - transient working files, will be recreated
* `./visualapp/*` - binaries to run Visual simulator
* `./Visualtest2/*` - project to generate random test data files


