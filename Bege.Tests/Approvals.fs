module Approvals

open System.IO

open ApprovalTests
open ApprovalTests.Reporters
open Xunit

open Bege.Compiler
open Bege.Options

let getOutput fileName optimize =
    let code = File.ReadAllText fileName
    
    let options =
        { optimize = optimize
        ; standard = befunge98
        ; verbose = false
        }

    let factory = compile options code

    let output = new StringWriter ()
    let funge = factory.create(new StringReader(""), output, uint64(System.Guid.NewGuid().GetHashCode()))
    funge.Run() |> ignore
    
    output.ToString()

[<Fact>]
let ``Mycology sanity test`` () =
    Approvals.Verify(getOutput "sanity.bf" true)

[<Fact>]
let ``Mycology big test`` () = 
    Approvals.Verify(getOutput "mycology.b98" true)

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