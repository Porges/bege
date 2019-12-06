// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Bege.AST

open System
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
    | Push | Pop
    | Discard | Clear
    | Dup | Flip
    | OutputNumber
    | InputNumber
    | OutputChar
    | InputChar
    | BinOp of BinOp
    | UnOp of UnOp

module LastInstruction =

    type LastInstruction<'a when 'a : comparison> =
        private
        | Exit
        | ToState of next : 'a
        | Branch of zero : 'a * nonZero : 'a
        | Rand of 'a list

    let (|Exit|ToState|Branch|Rand|) = function
        | Exit -> Exit
        | ToState n -> ToState n
        | Branch (z, nz) -> Branch (z, nz)
        | Rand ts -> Rand ts

    let exit = Exit
    let toState = ToState
    let branch z nz = Branch (z, nz)
    let rand<'a when 'a : comparison> (ts : IEnumerable<'a>) = 
        let dts = ts.Distinct().ToList()
        dts.Sort()
        Rand (List.ofSeq dts)

    // LastInstruction is a functor
    let map f = function
        | Exit -> exit
        | ToState t -> toState (f t)
        | Branch (z, nz) -> branch (f z) (f nz)
        | Rand ts -> rand (ts.Select(f))

open LastInstruction

type Chain<'a when 'a : comparison> = Instruction list * LastInstruction<'a>
type Program<'a when 'a : comparison> = Map<'a, Chain<'a>>

let computeChains (prog : Parser.Program) (options : Options) : Program<InstructionPointer.State> =

    let checkYear y command = 
        if y > options.standard.year
        then
            let allowedIn = { options.standard with year = y }
            let msg = sprintf "The command '%c' is only available in %A." command allowedIn
            raise <| FatalException(msg, ExitCodes.CommandNotSupported)
    
    let toCompile = System.Collections.Generic.Queue<InstructionPointer.State>()
    let visited = System.Collections.Generic.HashSet<InstructionPointer.State>()
    let mutable chains = Map.empty

    let toVisit newState =
        if visited.Add newState
        then toCompile.Enqueue newState

    toVisit programEntryState

    let read (struct (x, y)) = prog.[x, y]

    let rec follow chain (ip : InstructionPointer.State) : Chain<InstructionPointer.State> = 

        let go xs =
            follow (List.append (List.rev xs) chain) (advance ip)

        let branchChain(delta) =
            let next = advance { ip with delta = delta }
            toVisit next
            next

        let endChainWith c x = (List.rev (c :: chain), x)
        let endChain x = (List.rev chain, x)

        let newChain delta = 
            let next = advance { ip with delta = delta }
            toVisit next
            endChain (toState next)

        let binOp o =
            go [ Pop; Pop; BinOp o; Push]

        let unOp o =
            go [ Pop; UnOp o; Push ]

        match char (read ip.position) with
        | '>' -> newChain Dir.right
        | '<' -> newChain Dir.left
        | '^' -> newChain Dir.up
        | 'v' -> newChain Dir.down
        | d when d >= '0' && d <= '9' -> go [Load (int(d) - int('0')); Push]
        | h when h >= 'a' && h <= 'f' -> checkYear 98 h ; go [Load (int(h) - int('a') + 10); Push]
        | '$' -> go [Pop; Discard]
        | ' ' -> follow chain (advance ip)
        | '#' -> follow chain (advance (advance ip))
        | '"' -> readString chain (advance ip)
        | '@' -> endChain exit
        | '~' -> go [InputChar; Push]
        | '&' -> go [InputNumber; Push]
        | ':' -> go [Pop; Dup; Push; Push]
        | '\\' -> go [Pop; Pop; Flip; Push; Push]
        | '.' -> go [Pop; OutputNumber]
        | ',' -> go [Pop; OutputChar]
        | '!' -> unOp Not
        | '?' -> endChain (rand [branchChain Dir.left; branchChain Dir.right; branchChain Dir.up; branchChain Dir.down])
        | '+' -> binOp Add
        | '-' -> binOp Subtract
        | '/' -> binOp Divide
        | '%' -> binOp Modulo
        | '*' -> binOp Multiply
        | '_' -> endChainWith Pop (branch (branchChain Dir.right) (branchChain Dir.left))
        | '|' -> endChainWith Pop (branch (branchChain Dir.down) (branchChain Dir.up))
        | '`' -> binOp Greater
        | 'g' -> binOp ReadText
        | ';' -> checkYear 98 ';'; readComment chain (advance ip)
        | 'n' -> checkYear 98 'n'; go [Clear]
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
            | Push :: Load 32 :: _ -> readString acc (advance state)
            | _ -> readString (Push :: Load 32 :: acc) (advance state)
        | c -> readString (Push :: Load (int c) :: acc) (advance state)

    and readComment chain (state : InstructionPointer.State) =
        match char (read state.position) with
        | ';' -> follow chain (advance state)
        | c -> readComment chain (advance state)

    while toCompile.Count > 0 do
    
        let start = toCompile.Dequeue()
        chains <- Map.add start (follow [] start) chains

    chains
