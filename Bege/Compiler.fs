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

let compile (options : Options) (text : string) : Type =
    let prog = Parser.parse text
    computeChains prog options
    |> (fun p -> if options.optimize then optimize prog p else p)
    |> buildType options.outputFileName text

let run prog (seed : uint64) (input : TextReader) (output : TextWriter) options =
    let compiled = compile prog options
    
    let instance =
        Activator.CreateInstance(compiled, input, output, seed)
        :?> BefungeBase
    
    instance.Run()
