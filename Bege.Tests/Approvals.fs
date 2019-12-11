namespace Bege.Tests

open System
open System.IO
open System.Threading

open ApprovalTests
open ApprovalTests.Reporters
open Xunit

open Bege.Compiler
open Bege.Options

type Approval (itoh: Xunit.Abstractions.ITestOutputHelper) =
    inherit Support.TestBase(itoh)
    let verbose = base.Verbose

    let getOutput fileName optimize =
        let code = File.ReadAllText fileName
        
        let options =
            { optimize = optimize
            ; standard = Standard.Befunge98
            ; verbose = verbose
            }

        let factory = compile options code

        let output = new StringWriter ()
        let funge = factory.create (new StringReader("")) output (uint64(System.Guid.NewGuid().GetHashCode()))
        funge.Run() |> ignore
        
        output.ToString()

    [<Fact>]
    let ``Mycology sanity test`` () =
        Approvals.Verify (getOutput "sanity.bf" true)
        
    [<Fact>]
    let ``Mycology sanity test unoptimized`` () =
        Approvals.Verify (getOutput "sanity.bf" false)
        
    [<Fact>]
    let ``Mycology big test`` () = 
        Approvals.Verify (getOutput "mycology.b98" true)
        
    [<Fact>]
    let ``Mycology big test unoptimized`` () = 
        Approvals.Verify (getOutput "mycology.b98" false)
        
    [<Fact(Skip="Needs 'p' to work")>]
    let ``Mycology mycorand test`` () =
        Approvals.Verify (getOutput "mycorand.bf" true)
            
    [<Fact(Skip="Needs 'p' to work")>]
    let ``Mycology mycorand test unoptimized`` () =
        Approvals.Verify (getOutput "mycorand.bf" false)

    [<Fact>]
    let ``Catseye tests`` () =
        let files =
            [ "beer.bf"
            //; "befunge1.bf"
            ; "befunge2.bf"
            //; "befunge3.bf"
            //; "befunge4.bf"
            //; "befungex.bf"
            ; "hello.bf"
            ; "pi.bf"
            //; "pi2.bf"
            ]
        
        let results = List.map (fun f -> (f, getOutput f true)) files |> dict

        Approvals.VerifyAll(results)

[<assembly: IgnoreLineEndingsAttribute(true)>]
[<assembly: UseReporter(typeof<ApprovalTests.Reporters.DiffReporter>)>]
[<assembly: Namers.UseApprovalSubdirectory("approvals")>]
do()