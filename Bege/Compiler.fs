module Bege.Compiler

open Bege.Parser
open Bege.AST
open Bege.Renderer
open Bege.Runtime
open Bege.Optimizer

open System
open System.IO
open System.Text

let toText (prog : Parser.Program) : string =
    let sb = StringBuilder(Array2D.length1 prog * Array2D.length2 prog)
    prog |> Array2D.iter (fun c -> sb.Append c |> ignore)
    sb.ToString()

let compile (fileName : string option) (prog : Parser.Program) shouldOptimize : Type =
    computeChains prog 
    |> (fun p -> if shouldOptimize then optimize p else p)
    |> buildType fileName (toText prog)

let run prog (seed : uint64) (input : TextReader) (output : TextWriter) =
    let compiled = compile None prog true
    
    let instance =
        Activator.CreateInstance(compiled, input, output, seed)
        :?> BefungeBase
    
    instance.Run()
