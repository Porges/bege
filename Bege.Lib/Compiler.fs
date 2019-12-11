module Bege.Compiler

open Bege.InstructionPointer
open Bege.Parser
open Bege.Renderer
open Bege.Runtime
open Bege.Optimizer
open Bege.Options

open System
open System.IO

let private nameFromIP { delta = d; position = struct (x, y) } = sprintf "%A_%d_%d" d x y
let private nameFromIPAndStack ((ip, stack) : StateId) = sprintf "%s_%A" (nameFromIP ip) stack
// boo, `with` can't change generic type, so we have to write out the whole thing
let private updateInstructions f c =
    { chain = { instructions = c.chain.instructions; control = Control.map f c.chain.control }
    ; behaviour = c.behaviour }

let private mapKV kf vf m = Map.fold (fun m k v -> Map.add (kf k) (vf v) m) Map.empty m

// The optimization and non-optimization paths produce different types:
// this merges them together by forming the state names into strings for both.
let private optimizeAndMapToCommonType options (program: Program<_>) : Map<string, TypedChain<string>> * string =
    if options.optimize
    then
        let program = optimize options program |> mapKV nameFromIPAndStack (updateInstructions nameFromIPAndStack)
        let entryPoint = nameFromIPAndStack (programEntryState, emptyStack)
        (program, entryPoint)
    else
        let program = program.chains |> mapKV nameFromIP (fun v -> { chain = { instructions = v.instructions; control = Control.map nameFromIP v.control }; behaviour = (0,0) })
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
    let memory = Loader.load text
    memory
    |> parse options
    |> optimizeAndMapToCommonType options
    |> buildType options text
    |> FungeFactory
