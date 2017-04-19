module Bege.Compiler

open Bege.Parser
open Bege.AST
open Bege.InstructionPointer
open Bege.Renderer
open Bege.Runtime
open Bege.Optimizer
open Bege.Options

open System
open System.IO
open System.Text

let nameFromIP { delta = d; position = struct (x, y) } = sprintf "%A_%d_%d" d x y
let nameFromIPAndStack (ip, stack) = sprintf "%s_%A" (nameFromIP ip) stack
let updateInstructions f (insns, last) = (insns, LastInstruction.map f last)
let mapKV kf vf m = Map.fold (fun m k v -> Map.add (kf k) (vf v) m) Map.empty m

let compile (options : Options) (text : string) : Type =

    let prog = Parser.parse text
    computeChains prog options
    |> (fun p ->
        if options.optimize
        then (mapKV nameFromIPAndStack (updateInstructions nameFromIPAndStack) (optimize prog p), nameFromIPAndStack (programEntryState, EmptyStack))
        else (mapKV nameFromIP (updateInstructions nameFromIP) p), nameFromIP programEntryState)
    |> buildType options.outputFileName text

let run prog (seed : uint64) (input : TextReader) (output : TextWriter) options =
    let compiled = compile prog options
    
    let instance =
        Activator.CreateInstance(compiled, input, output, seed)
        :?> BefungeBase
    
    instance.Run()
