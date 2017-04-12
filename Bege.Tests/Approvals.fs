module Approvals

open System.Diagnostics
open System.IO
open System.Threading.Tasks

open ApprovalTests
open ApprovalTests.Reporters
open Xunit

open Bege.Compiler
open Bege.Options
open System.Threading
open System

let getOutput fileName optimize =
    let code = File.ReadAllText fileName

    let outputPath = Path.ChangeExtension(fileName, ".exe")

    let options =
        { outputFileName = Some outputPath
        ; optimize = optimize
        ; standard = befunge98
        }

    compile options code |> ignore

    let output = 
        let psi = ProcessStartInfo(outputPath, RedirectStandardOutput = true, UseShellExecute = false)
        use cts = new CancellationTokenSource(1000)
        use p = Process.Start psi
        use reg = cts.Token.Register(fun () -> p.Kill())
        p.StandardOutput.ReadToEnd()

    output

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
        //; "pi.bf"
        //; "pi2.bf"
        ]
    
    let results = List.map (fun f -> (f, getOutput f true)) files |> dict

    Approvals.VerifyAll(results)

[<assembly: UseReporter(typeof<ApprovalTests.Reporters.DiffReporter>)>]
[<assembly: Namers.UseApprovalSubdirectory("approvals")>]
do()