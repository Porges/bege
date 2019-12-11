// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Bege.Parser

open System.Linq
open System.Collections.Generic

open Bege.InstructionPointer
open Bege.Options

type BinOp = 
    | Add | Multiply | Divide | Subtract | Greater | ReadText | Modulo

type UnOp =
    | Not

type Instruction = 
    | Load of int
    | Discard | Clear
    | Dup | Flip
    | OutputNumber
    | InputNumber
    | OutputChar
    | InputChar
    | BinOp of BinOp
    | UnOp of UnOp

[<AutoOpen>]
module Control =

    [<RequireQualifiedAccess>]
    type Control<'a when 'a : comparison> =
        private
        | Exit
        | ToState of next : 'a
        | Branch of zero : 'a * nonZero : 'a
        | Rand of 'a list

    let (|Exit|ToState|Branch|Rand|) = function
        | Control.Exit -> Exit
        | Control.ToState n -> ToState n
        | Control.Branch (z, nz) -> Branch (z, nz)
        | Control.Rand ts -> Rand ts

    let Exit = Control.Exit
    let ToState = Control.ToState
    let Branch z nz = Control.Branch (z, nz)
    let Rand<'a when 'a : comparison> (ts : IEnumerable<'a>) = 
        Control.Rand (ts|> Seq.distinct |> Seq.sort |> List.ofSeq)

    // Control is a functor
    let map f = function
        | Exit -> Exit
        | ToState t -> ToState (f t)
        | Branch (z, nz) -> Branch (f z) (f nz)
        | Rand ts -> Rand (ts.Select(f))

    let iter f = function
        | Exit -> ()
        | ToState t -> f t
        | Branch (z, nz) -> f z; f nz
        | Rand ts -> List.iter f ts

open Control

type Chain<'a when 'a : comparison> = { instructions: Instruction list; control: Control<'a> }
type Program<'a when 'a : comparison> = { memory: BefungeSpace; chains: Map<'a, Chain<'a>> }

let parseChain
    (options: Options)
    (memory: BefungeSpace)
    (start: InstructionPointer.State): Chain<InstructionPointer.State> =

    let checkYear y command = 
        if y > options.standard.year
        then
            let allowedIn = { options.standard with year = y }
            let msg = sprintf "The command '%c' is only available in %A." command allowedIn
            raise <| FatalException(msg, ExitCodes.CommandNotSupported)

    let read (struct (x, y)) = memory.[x, y]

    let rec follow instructions (ip : InstructionPointer.State) : Chain<InstructionPointer.State> = 

        let go x = follow (x :: instructions) (advance ip)

        let branchChain delta =
            let next = advance { ip with delta = delta }
            next

        let endChain c = { instructions = List.rev instructions; control = c }

        let newChain delta = 
            let next = advance { ip with delta = delta }
            endChain (ToState next)
            
        match char (read ip.position) with
        | '>' -> newChain Dir.right
        | '<' -> newChain Dir.left
        | '^' -> newChain Dir.up
        | 'v' -> newChain Dir.down
        | d when d >= '0' && d <= '9' -> go (Load (int(d) - int('0')))
        | h when h >= 'a' && h <= 'f' -> checkYear 98 h ; go (Load (int(h) - int('a') + 10))
        | '$' -> go Discard
        | ' ' -> follow instructions (advance ip)
        | '#' -> follow instructions (advance (advance ip))
        | '"' -> readString instructions (advance ip)
        | '@' -> endChain Exit
        | '~' -> go InputChar
        | '&' -> go InputNumber 
        | ':' -> go Dup
        | '\\' -> go Flip
        | '.' -> go OutputNumber
        | ',' -> go OutputChar
        | '!' -> go (UnOp Not)
        | '?' -> endChain (Rand [branchChain Dir.left; branchChain Dir.right; branchChain Dir.up; branchChain Dir.down])
        | '+' -> go (BinOp Add)
        | '-' -> go (BinOp Subtract)
        | '/' -> go (BinOp Divide)
        | '%' -> go (BinOp Modulo)
        | '*' -> go (BinOp Multiply)
        | '_' -> endChain (Branch (branchChain Dir.right) (branchChain Dir.left))
        | '|' -> endChain (Branch (branchChain Dir.down) (branchChain Dir.up))
        | '`' -> go (BinOp Greater)
        | 'g' -> go (BinOp ReadText)
        | ';' -> checkYear 98 ';'; readComment instructions (advance ip)
        | 'n' -> checkYear 98 'n'; go Clear
        | c -> 
            if options.standard.year = 93
            then raise <| FatalException("Instruction not supported: " + string(c), ExitCodes.CommandNotSupported)
            else 
                if c <> 'r'
                then eprintf "Warning: unsupported instruction '%c', treating as if 'r'." c
                newChain (reflect ip.delta)
             
    and readString acc (state : InstructionPointer.State) : Chain<InstructionPointer.State> =
        match char (read state.position) with
        | '"' -> follow acc (advance state)
        | ' ' when options.standard.year = 98 ->
            match acc with
            | Load 32 :: _ -> readString acc (advance state)
            | _ -> readString (Load 32 :: acc) (advance state)
        | c -> readString (Load (int c) :: acc) (advance state)

    and readComment chain (state : InstructionPointer.State) =
        match char (read state.position) with
        | ';' -> follow chain (advance state)
        | c -> readComment chain (advance state)

    follow [] start


let parse (options : Options) (memory : BefungeSpace) : Program<InstructionPointer.State> =
    
    let toCompile = System.Collections.Generic.Queue<InstructionPointer.State>()
    let visited = System.Collections.Generic.HashSet<InstructionPointer.State>()
    let mutable chains = Map.empty

    let toVisit newState =
        if visited.Add newState
        then toCompile.Enqueue newState

    toVisit programEntryState

    while toCompile.Count > 0 do
        let start = toCompile.Dequeue()
        let chain = parseChain options memory start
        chains <- Map.add start chain chains

        Control.iter toVisit chain.control

    { memory = memory; chains = chains }
