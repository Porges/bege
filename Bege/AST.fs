// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Bege.AST

open System.Text
open Bege.Common
open Bege.Options

/// Sorts a tuple of 4 elements
let sort4 (a, b, c, d) =
    let [x; y; z; w] = List.sort [a; b; c; d]
    (x, y, z, w)

/// Instruction pointer direction
type Dir = Right = 0 | Up = 1 | Down = 2 | Left = 3
// (Important that Right is first so the initial instruction pointer
//  sorts before others at the same position...)

/// Instruction pointer state
[<StructuredFormatDisplay("{dir} {position}")>]
type [<Struct>] IPState = { position : struct (int * int) ; dir : Dir }

/// Initial pointer state
let programEntryState = { position = struct (0,0); dir = Dir.Right }

let advance (ip : IPState) = 
    let struct (x, y) = ip.position
    let (x', y') =
        match ip.dir with
        | Dir.Right -> (x, y+1)
        | Dir.Left -> (x, y-1)
        | Dir.Up -> (x-1, y)
        | Dir.Down -> (x+1, y)
    let x' = if x' < 0 then x' + 25 else x' % 25
    let y' = if y' < 0 then y' + 80 else y' % 80
    { ip with position = struct (x', y') }

type BinOp = 
    | Add | Multiply | Divide | Subtract | Greater

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
    | ReadText
    | BinOp of BinOp
    | UnOp of UnOp

type LastInstruction =
    | Exit
    | ToState of next : IPState
    | Branch of zero : IPState * nonZero : IPState
    | Rand of IPState * IPState * IPState * IPState

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

            match read ip.position with
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
            | '?' -> endChain (Rand (sort4 (branchChain Dir.Left, branchChain Dir.Right, branchChain Dir.Up, branchChain Dir.Down)))
            | '+' -> binOp Add
            | '-' -> binOp Subtract
            | '/' -> binOp Divide
            | '*' -> binOp Multiply
            | '_' -> endChainWith Pop (Branch (branchChain Dir.Right, branchChain Dir.Left))
            | '|' -> endChainWith Pop (Branch (branchChain Dir.Up, branchChain Dir.Down))
            | '`' -> binOp Greater
            | 'g' -> go [Pop; Pop; ReadText; Push]
            | ';' -> checkYear 98 ';'; readComment chain (advance ip)
            | c -> raise <| FatalException("Instruction not supported: " + string(c), ExitCodes.CommandNotSupported)
        and readString acc (state : IPState) : (Instruction list * LastInstruction) =
            match read state.position with
            | '"' -> follow acc (advance state)
            | c -> readString (Push :: Load (int c) :: acc) (advance state)
        and readComment chain (state : IPState) =
            match read state.position with
            | ';' -> follow chain (advance state)
            | c -> readComment chain (advance state)
        
        let start = toCompile.Dequeue()
        chains <- Map.add start (follow [] start) chains

    chains
