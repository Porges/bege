module Bege.Compiler

open Bege.Parser
open Bege.AST
open Bege.Renderer
open Bege.Runtime
open Bege.Optimizer
open Bege.Options

open System
open System.IO
open System.Text

let toText (prog : Parser.Program) : string =
    let sb = StringBuilder(Array2D.length1 prog * Array2D.length2 prog)
    prog |> Array2D.iter (fun c -> sb.Append c |> ignore)
    sb.ToString()

let compile (options : Options) (prog : Parser.Program) : Type =
    computeChains prog options
    |> (fun p -> if options.optimize then optimize p else p)
    |> buildType options.outputFileName (toText prog)

let run prog (seed : uint64) (input : TextReader) (output : TextWriter) options =
    let compiled = compile prog options
    
    let instance =
        Activator.CreateInstance(compiled, input, output, seed)
        :?> BefungeBase
    
    instance.Run()
