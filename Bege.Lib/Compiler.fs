﻿module Bege.Compiler

open Bege.AST
open Bege.InstructionPointer
open Bege.Renderer
open Bege.Runtime
open Bege.Optimizer
open Bege.Options

open System
open System.IO

let private nameFromIP { delta = d; position = struct (x, y) } = sprintf "%A_%d_%d" d x y
let private nameFromIPAndStack ((ip, stack) : StateId) = sprintf "%s_%A" (nameFromIP ip) stack
// boo, `with` can't change generic type!
let private updateInstructions f c = { instructions = c.instructions; lastInstruction = LastInstruction.map f c.lastInstruction; stackBehaviour = c.stackBehaviour }
let private mapKV kf vf m = Map.fold (fun m k v -> Map.add (kf k) (vf v) m) Map.empty m

let private optimizeAndMapToCommonType options programText p : Map<string, TypedChain<string>> * string =
    if options.optimize
    then
        let program = optimize options programText p |> mapKV nameFromIPAndStack (updateInstructions nameFromIPAndStack)
        let entryPoint = nameFromIPAndStack (programEntryState, EmptyStack)
        (program, entryPoint)
    else
        let program = p |> mapKV nameFromIP (fun v -> { instructions = fst v; lastInstruction = LastInstruction.map nameFromIP (snd v); stackBehaviour = (0,0) })
        let entryPoint = nameFromIP programEntryState
        (program, entryPoint)

type FungeFactory internal (it: Type) =
    [<CompiledName("Create")>]
    member _.createDefault (): Funge =
        Activator.CreateInstance(it) :?> Funge
 
    [<CompiledName("Create")>]
    member _.create (input: TextReader) (output: TextWriter) (seed: uint64): Funge =
        Activator.CreateInstance(it, input, output, seed) :?> Funge

[<CompiledName("Compile")>]
let compile (options : Options) (text : string) : FungeFactory =
    let prog = Parser.parse text
    computeChains prog options
    |> optimizeAndMapToCommonType options prog
    |> buildType options text
    |> FungeFactory
