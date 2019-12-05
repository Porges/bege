﻿module Bege.Optimizer

open Bege.AST
open Bege.Common
open Bege.InstructionPointer
open Bege.Options

type StackValue =
    /// A value that is statically known.
    | Known of int
    /// An unknown value (e.g. read from input).
    | Unknown
    /// A dead (unused) value, we will kill any discard it encounters.
    | KillDiscard

type Stack
    = UnknownStack
    | EmptyStack
    | Value of StackValue * Stack
    // | UseLocal of Stack // TODO: how to do this?

/// How many items the chain pops, and how many it stores.
type StackBehaviour = int * int
type LocalStack = StackValue list
type StateId = IPState * Stack

type TypedChain<'a> =
    { instructions : Instruction list
    ; stackBehaviour : StackBehaviour
    ; lastInstruction  : LastInstruction<'a>
    }

type TypedProgram = Map<StateId, TypedChain<StateId>>

let inlineChains (options : Options) (program : TypedProgram) : TypedProgram =

    let inc k m =
        match Map.tryFind k m with
        | None -> Map.add k 1 m
        | Some c -> Map.add k (c+1) m

    let countReferences m =
            m |> Map.fold (fun cs _ c ->
                match c.lastInstruction with
                | Exit -> cs
                | Branch (l, r) -> inc l (inc r cs)
                | Rand targets -> List.fold (fun cs x -> inc x cs) cs targets
                | ToState n -> inc n cs) Map.empty

    let counts = countReferences program

    let canBeInlined fs =
        counts.[fs] = 1

    let isJump fs =
        match program.[fs] with
        | { instructions = []; lastInstruction = ToState t } -> true
        | _ -> false

    let target fs =
        match program.[fs] with
        | { instructions = []; lastInstruction = ToState t } -> t
        | _ -> failwith "target must be guarded by isJump"

    let result =
        program
        |> Map.map (fun fs c ->
            match c.lastInstruction with
            | ToState n when canBeInlined n || isJump n ->
                // if options.verbose then printfn "Inlining %A into %A" n fs
                let targetChain = program.[n]
                { instructions = (List.append c.instructions targetChain.instructions)
                ; lastInstruction = targetChain.lastInstruction
                ; stackBehaviour = c.stackBehaviour // TODO: this is wrong!
                }
            | Branch (z, nz) when isJump z || isJump nz ->
                let z = if isJump z then target z else z
                let nz = if isJump nz then target nz else nz
                { c with lastInstruction = Branch (z, nz) }
            | Rand targets
                when List.exists isJump targets ->
                let newTargets = List.map (fun x -> if isJump x then target x else x) targets
                { c with lastInstruction = Rand (List.sort newTargets) }
            | _ ->  c)

    let newCounts = countReferences result

    let trimmedResult =
        result
        (* can't remove entry state *)
        |> Map.filter (fun ((ip, s) as k) _ -> newCounts.ContainsKey k || (ip = programEntryState && s = EmptyStack))

    if options.verbose
    then printfn "Inlining reduced chains from %d to %d." program.Count trimmedResult.Count

    trimmedResult

let rec peepholeOptimize (prog : Parser.Program) = function
    // Unneeded push/pops
    | Push :: Pop :: is -> peepholeOptimize prog is
    | Dup :: Pop :: is -> peepholeOptimize prog is
    | Load _ :: Discard :: is -> peepholeOptimize prog is
    // eliminate unneeded nots
    | UnOp Not :: UnOp Not :: is -> peepholeOptimize prog is
    // eliminate unneeded flips
    | Flip :: Flip :: is -> peepholeOptimize prog is
    | Dup :: Flip :: is -> Dup :: peepholeOptimize prog is
    | (i :: is) -> i :: peepholeOptimize prog is
    | [] -> []

type EventualFate = Pushed | Consumed | Discarded

/// Figures out the eventual fate for a value that
/// was just pushed onto the stack:
let eventualFate lastInstruction instructions =
    /// `st` = number of items on stack above the value.
    let rec eventualFateLocal st = function
        | [] ->
            if st <> 0
            then failwith "Ended up on local stack"
            else Consumed // there must be a branch next - TODO: check this
        | Clear :: is -> failwith "Cleared while on local stack"
        | Load k :: is -> eventualFateLocal (st + 1) is
        | Push :: is ->
            if st = 0
            then eventualFateG 0 is
            else eventualFateLocal (st - 1) is
        | Pop :: is -> eventualFateLocal (st + 1) is
        | InputNumber :: is
        | InputChar :: is -> eventualFateLocal (st + 1) is
        | UnOp _ :: is
        | OutputChar :: is
        | OutputNumber :: is ->
            if st = 0
            then Consumed
            else eventualFateLocal (st - 1) is
        | Dup :: is ->
            if st = 0
            then Consumed
            else eventualFateLocal (st - 1) is
        | Flip :: is ->
            match st with
            // TODO: think about this, fix and restore
            //| 0 -> eventualFateLocal (st + 1) is
            //| 1 -> eventualFateLocal 0 is
            | 0 | 1 -> Consumed
            | _ -> eventualFateLocal st is
        | Discard :: is ->
            if st = 0
            then Discarded
            else eventualFateLocal (st - 1) is
        | BinOp op :: is ->
            if st < 2
            then Consumed
            else eventualFateLocal (st - 1) is

    and eventualFateG st = function
        | [] -> Pushed
        | Pop :: is ->
            if st = 0
            then eventualFateLocal 0 is
            else eventualFateG (st - 1) is
        | Push :: is -> eventualFateG (st + 1) is
        | Clear :: is -> Discarded
        | _ :: is -> eventualFateG st is

    eventualFateLocal 0 instructions

let rec performStackAnalysis (program : Parser.Program) ((ip, initStack) : StateId) ((insns, last) : Chain<StateId>) : TypedChain<StateId> =

    // acc = instruction acculuator
    // gs = global stack
    // ls = local stack
    // read = how many items read from stack
    let rec go acc gs read ls = function
        | [] ->
            let rec finale wrote = function
                | Value (v, rest) -> finale (wrote+1) rest

                | EmptyStack ->
                    let (insns, lastInsn) =
                        if wrote = 0
                        // we know it's empty so we can propagate that information
                        // to the next chain.
                        then (List.rev acc, LastInstruction.map (fun (ip, st) -> (ip, EmptyStack)) last)
                        else (List.rev acc, last)

                    if read <> 0
                    then failwith "Logic problem - claiming read values for known empty stack"

                    { instructions = insns
                    ; lastInstruction = lastInsn
                    ; stackBehaviour = (0, wrote)
                    }

                | UnknownStack ->

                    match last with
                    | Branch _ ->
                        if List.length ls <> 1
                        then failwith "Branch without push before it"
                    | _ ->
                        if not (List.isEmpty ls)
                        then failwith "Function ended with non-empty local stack"

                    { instructions = List.rev acc
                    ; lastInstruction = last
                    ; stackBehaviour = (read, wrote)
                    }

            finale 0 gs

        | Load k :: is ->
            match eventualFate last is with
            | Discarded -> go acc gs read (KillDiscard :: ls) is
            | _ -> go (Load k :: acc) gs read (Known k :: ls) is

        | Push :: is ->
            match ls with
            | KillDiscard :: ls -> go acc (Value (KillDiscard, gs)) read ls is
            | l :: ls -> go (Push :: acc) (Value (l, gs)) read ls is
            | [] -> failwith "Pushed from empty local stack"

        | Pop :: is ->
            match gs with
            | EmptyStack -> go (Load 0 :: acc) EmptyStack read (Known 0 :: ls) is
            | UnknownStack -> go (Pop :: acc) UnknownStack (read+1) (Unknown :: ls) is
            | Value (Known x, rest) -> go (Load x :: Discard :: Pop :: acc) rest read (Known x :: ls) is
            | Value (Unknown, rest) -> go (Pop :: acc) rest read (Unknown :: ls) is
            | Value (KillDiscard, rest) -> go acc rest read (KillDiscard :: ls) is
            //| UseLocal rest -> go acc rest ls is

        | Discard :: is ->
            match ls with
            | KillDiscard :: ls -> go acc gs read ls is
            | _ :: ls -> go (Discard :: acc) gs read ls is
            | [] -> failwith (sprintf "Unexpected empty stack for discard.")

        | (OutputChar as i) :: is
        | (OutputNumber as i) :: is ->
            match ls with
            | (_ :: ls) -> go (i :: acc) gs read ls is
            | [] -> failwith (sprintf "Unexpected empty stack for %A instruction" i)

        | Clear :: is ->
            match ls with
            | [] -> go (Clear :: acc) EmptyStack read ls is
            | _ -> failwith "Clear with unclear local stack"

        | BinOp op as bop :: is ->
            match ls with
            | Known b :: Known a :: ls ->
                let result =
                    match op with
                    | Multiply -> b * a
                    | Add -> b + a
                    | Subtract -> b - a
                    | Divide -> b / a
                    | Modulo -> b % a
                    | ReadText -> program.[a, b]
                    | Greater -> if b > a then 1 else 0
                go (Load result :: Discard :: Discard :: acc) gs read (Known result :: ls) is

            | Known 0 :: x :: ls
            | x :: Known 0 :: ls ->
                match (op, x) with
                | (Multiply, _) ->
                    go (Load 0 :: Discard :: Discard :: acc) gs read (Known 0 :: ls) is

                | (Add, Known x) ->
                    go (Load x :: Discard :: Discard :: acc) gs read (Known x :: ls) is

                | _ -> go (bop :: acc) gs read (Unknown :: ls) is

            | Known 1 :: x :: ls
            | x :: Known 1 :: ls ->
                match (op, x) with
                | (Multiply, Known x) ->
                    go (Load x :: Discard :: Discard :: acc) gs read (Known x :: ls) is

                | _ -> go (bop :: acc) gs read (Unknown :: ls) is

            | _ :: _ :: ls -> go (bop :: acc) gs read (Unknown :: ls) is // unknown result
            | _ -> failwith "Binary operation without enough arguments on stack"

        | UnOp op :: is ->
            match ls with
            | Known x :: ls ->
                let result =
                    match op with
                    | Not -> if x = 0 then 1 else 0

                go (Load result :: Discard :: acc) gs read (Known result :: ls) is

            | Unknown :: ls -> go (UnOp op :: acc) gs read (Unknown :: ls) is // unknown result
            | _ -> failwith "Unary operation without enough arguments on stack"

        | Dup :: is ->
            match ls with
            | (v :: _) -> go (Dup :: acc) gs read (v :: ls) is
            | [] -> failwith "Dup with empty stack"

        | Flip :: is ->
            match ls with
            | (Known x :: Known y :: ls) -> go (Load y :: Load x :: Discard :: Discard :: acc) gs read (Known y :: Known x :: ls) is
            | (x :: y :: ls) -> go (Flip :: acc) gs read (y :: x :: ls) is
            | _ -> failwith "Flip without enough arguments on stack"

        | (InputChar as i) :: is
        | (InputNumber as i) :: is -> go (i :: acc) gs read (Unknown :: ls) is

    go [] initStack 0 [] insns

let optimizeLast fs last instructions =
    let noInstructions = List.isEmpty instructions

    match last with
    // identical branches - change to unconditional jump
    // we must add Discard to get rid of the existing Pop
    | Branch (l, r) when l = r -> (List.append instructions [Discard], ToState r)

    // TODO: update these patterns to deal with N cases
    // Rand with all identical:
    | Rand [a; b; c; d] when a = b && b = c && c = d -> (instructions, ToState d)

    // Rand that loops back to same function must eventually branch to another,
    // so if there are no instructions in this function we could just jump instead.
    //
    // There are two orders to check here - since Rand is always ordered (via ord4),
    // either fs < all or fs > all:
    | Rand [a; b; c; d] when noInstructions && a = fs && b = c && c = d -> (instructions, ToState d)
    | Rand [a; b; c; d] when noInstructions && a = fs && b = fs && c = d -> (instructions, ToState d)
    | Rand [a; b; c; d] when noInstructions && a = fs && b = fs && c = fs -> (instructions, ToState d)

    | Rand [a; b; c; d] when noInstructions && a = b && b = c && d = fs -> (instructions, ToState a)
    | Rand [a; b; c; d] when noInstructions && a = b && c = fs && d = fs -> (instructions, ToState a)
    | Rand [a; b; c; d] when noInstructions && b = fs && c = fs && d = fs -> (instructions, ToState a)

    | c -> (instructions, c)

let optimizeChain program stateId (chain : TypedChain<StateId>) =
    fix (peepholeOptimize program) chain.instructions
    |> optimizeLast stateId chain.lastInstruction
    |> performStackAnalysis program stateId

/// performStackAnalysis can add references to "specializations"
/// that don't exist yet. This function creates them.
let instantiateSpecializations program =
    // copy the existing one with "default" stack if it doesn't already exist
    let copyIfNotExists ((ip, _) as state) m =
        if not (Map.containsKey state m)
        then Map.add state (m.[(ip, UnknownStack)]) m
        else m

    Map.fold (fun m k c ->
        match c.lastInstruction with
        | ToState t -> copyIfNotExists t m
        | Branch (z, nz) -> copyIfNotExists z (copyIfNotExists nz m)
        | Rand ts -> List.fold (fun m t -> copyIfNotExists t m) m ts
        | Exit -> m
        ) program program

let optimizeChains programText (program : TypedProgram) : TypedProgram =
    let newProgram = Map.map (optimizeChain programText) program
    instantiateSpecializations newProgram

// TODO: update to work with new types
let collapseIdenticalChains (m : Map<IPState, Instruction list * LastInstruction<IPState>>) : Map<IPState, Instruction list * LastInstruction<IPState>> =

    // invert the map, now all states with the same instruction list are in the same slot
    let inverted = invertMap m

    // pick one state to represent the others (the 'minimum')
    let newMappings : Map<IPState, IPState> =
        Map.fold (fun m _ fss ->
            let min = List.min fss in
            List.fold (fun m fs -> Map.add fs min m) m fss) Map.empty inverted

    // rewrite all the last-instructions to remap their states
    inverted
    |> Map.toSeq
    |> Seq.map (fun ((is, last), fss) ->
        let newLast =
            match last with
            | Exit -> Exit
            | Branch (one, two) -> Branch (newMappings.[one], newMappings.[two])
            | Rand targets -> Rand (List.sort (List.map (fun x -> newMappings.[x]) targets))
            | ToState n -> ToState (newMappings.[n])
        (List.min fss, (is, newLast)))
    |> Map.ofSeq

let augmentChain ((is, last) : Chain<IPState>) : Chain<StateId> =
    (is, LastInstruction.map (fun ipState -> (ipState, UnknownStack)) last)

let optimize (options : Options) (programText : Parser.Program) (chains : Program<IPState>)
    : TypedProgram =

    // add initial types (stack behaviours) to the program
    let typedProgram =
        chains |> Map.fold (fun m ipState v ->
            let stack =
                if ipState = programEntryState
                then EmptyStack
                else UnknownStack

            let typedChain = performStackAnalysis programText (ipState, stack) (augmentChain v)

            if options.verbose then printfn "%A has stack behaviour %A" (ipState, stack) typedChain.stackBehaviour

            Map.add (ipState, stack) typedChain m)
            Map.empty

    // optimize until we run out of optimizations
    instantiateSpecializations typedProgram
    |> fixN (fun n x ->
        if options.verbose then printfn "\nPerforming optimization pass %d" n
        optimizeChains programText (inlineChains options x))