// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Bege.AST

open System.Text
open Bege.Common
open Bege.InstructionPointer
open Bege.Options

type BinOp = 
    | Add | Multiply | Divide | Subtract | Greater | ReadText

type UnOp =
    | Not

type Instruction = 
    | Load of int
    | Push | Pop
    | Discard
    | Dup | Flip
    | OutputNumber
    | InputNumber
    | OutputChar
    | InputChar
    | BinOp of BinOp
    | UnOp of UnOp

type LastInstruction =
    | Exit
    | ToState of next : IPState
    | Branch of zero : IPState * nonZero : IPState
    | Rand of IPState list

type Chain = Instruction list * LastInstruction
type Program = Map<IPState, Instruction list * LastInstruction>

let computeChains (prog : Parser.Program) (options : Options) : Program =

    let checkYear y command = 
        if y > options.standard.year
        then
            let allowedIn = { options.standard with year = y }
            let msg = sprintf "The command '%c' is only available in %A." command allowedIn
            raise <| FatalException(msg, ExitCodes.CommandNotSupported)
    
    let toCompile = System.Collections.Generic.Queue<IPState>()
    let visited = System.Collections.Generic.HashSet<IPState>()
    let mutable chains = Map.empty

    let toVisit newState =
        if visited.Add newState
        then toCompile.Enqueue newState

    toVisit programEntryState

    let read (struct (x, y)) = prog.[x, y]

    while toCompile.Count > 0 do

        let rec follow chain (ip : IPState) : (Instruction list * LastInstruction) = 

            let go xs =
                follow (List.append (List.rev xs) chain) (advance ip)

            let branchChain(dir) =
                let next = advance { ip with dir = dir }
                toVisit next
                next

            let endChainWith c x = (List.rev (c :: chain), x)
            let endChain x = (List.rev chain, x)

            let newChain dir = 
                let next = advance { ip with dir = dir }
                toVisit next
                endChain (ToState next)

            let binOp o =
                go [ Pop; Pop; BinOp o; Push]

            let unOp o =
                go [ Pop; UnOp o; Push ]

            match char (read ip.position) with
            | '>' -> newChain Dir.Right
            | '<' -> newChain Dir.Left
            | '^' -> newChain Dir.Up
            | 'v' -> newChain Dir.Down
            | d when d >= '0' && d <= '9' -> go [Load (int(d) - int('0')); Push]
            | h when h >= 'a' && h <= 'f' -> checkYear 98 h ; go [Load (int(h) - int('a') + 10); Push]
            | '$' -> go [Pop; Discard]
            | ' ' -> follow chain (advance ip)
            | '#' -> follow chain (advance (advance ip))
            | '"' -> readString chain (advance ip)
            | '@' -> endChain Exit
            | '~' -> go [InputChar; Push]
            | '&' -> go [InputNumber; Push]
            | ':' -> go [Pop; Dup; Push; Push]
            | '\\' -> go [Pop; Pop; Flip; Push; Push]
            | '.' -> go [Pop; OutputNumber]
            | ',' -> go [Pop; OutputChar]
            | '!' -> unOp Not
            | '?' -> endChain (Rand (List.sort [branchChain Dir.Left; branchChain Dir.Right; branchChain Dir.Up; branchChain Dir.Down]))
            | '+' -> binOp Add
            | '-' -> binOp Subtract
            | '/' -> binOp Divide
            | '*' -> binOp Multiply
            | '_' -> endChainWith Pop (Branch (branchChain Dir.Right, branchChain Dir.Left))
            | '|' -> endChainWith Pop (Branch (branchChain Dir.Down, branchChain Dir.Up))
            | '`' -> binOp Greater
            | 'g' -> binOp ReadText
            | ';' -> checkYear 98 ';'; readComment chain (advance ip)
            | c -> 
                if options.standard.year = 93
                then raise <| FatalException("Instruction not supported: " + string(c), ExitCodes.CommandNotSupported)
                else 
                    eprintf "Warning: unsupported instruction '%c', treating as if 'r'." c
                    newChain (reflect ip.dir)
                 
        and readString acc (state : IPState) : (Instruction list * LastInstruction) =
            match char (read state.position) with
            | '"' -> follow acc (advance state)
            | c -> readString (Push :: Load (int c) :: acc) (advance state)
        and readComment chain (state : IPState) =
            match char (read state.position) with
            | ';' -> follow chain (advance state)
            | c -> readComment chain (advance state)
        
        let start = toCompile.Dequeue()
        chains <- Map.add start (follow [] start) chains

    chains
