module Bege.Optimizer

open Bege.AST
open Bege.AST.LastInstruction
open Bege.Common
open Bege.InstructionPointer
open Bege.Options

open System
open System.Linq

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
    | Push of StackValue * Stack
    
let push stk value =
    match (stk, value) with
    | (EmptyStack, Known 0) -> EmptyStack
    | (UnknownStack, Unknown) -> UnknownStack
    | (stk, value) -> Push (value, stk)

let pop = function
    | UnknownStack -> (Unknown, UnknownStack)
    | EmptyStack -> (Known 0, EmptyStack)
    | Push (KillDiscard, _) -> failwith "Unexpected KillDiscard"
    | Push (value, rest) -> (value, rest)
    
let popAllowDiscard = function
    | UnknownStack -> (Unknown, UnknownStack)
    | EmptyStack -> (Known 0, EmptyStack)
    | Push (value, rest) -> (value, rest)
    
let dup stk = 
    let (value, stk) = pop stk
    push (push stk value) value

/// How many items the chain pops, and how many it stores.
type StackBehaviour = int * int
type StateId = InstructionPointer.State * Stack

type TypedChain<'a when 'a : comparison> =
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
                | Rand targets -> Seq.fold (fun cs x -> inc x cs) cs targets
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
                { c with lastInstruction = branch z nz }
            | Rand targets
                when targets.Any(fun x -> isJump x) ->
                let newTargets = Seq.map (fun x -> if isJump x then target x else x) targets
                { c with lastInstruction = rand newTargets }
            | _ ->  c)

    let newCounts = countReferences result

    let trimmedResult =
        result
        (* can't remove entry state *)
        |> Map.filter (fun ((ip, s) as k) _ -> newCounts.ContainsKey k || (ip = programEntryState && s = EmptyStack))

    if options.verbose
    then printfn "Inlining reduced chains from %d to %d." program.Count trimmedResult.Count

    trimmedResult

let rec peepholeOptimize = function
    // dead loads
    | Load _ :: Discard :: is -> peepholeOptimize is
    | BinOp (ReadText _) :: Discard :: is -> Discard :: Discard :: peepholeOptimize is
    // unneeded discards
    | Discard :: Clear :: is -> Clear :: peepholeOptimize is
    | Clear :: Clear :: is -> Clear :: peepholeOptimize is
    // eliminate unneeded nots
    | UnOp Not :: UnOp Not :: is -> peepholeOptimize is
    // eliminate unneeded flips
    | Flip :: Flip :: is -> peepholeOptimize is
    | Dup :: Flip :: is -> Dup :: peepholeOptimize is
    // eliminated unneeded dups
    | Dup :: Discard :: is -> peepholeOptimize is

    | i :: is -> i :: peepholeOptimize is
    | [] -> []

type EventualFate = Pushed | Consumed | Discarded

let stackBehaviour: Instruction -> StackBehaviour = function
    | BinOp _ -> (2, 1)
    | UnOp _ -> (1, 1)
    | Discard -> (1, 0) 
    | Dup -> (1, 2)
    | Flip -> (2, 2)
    | InputChar -> (0, 1)
    | InputNumber -> (0, 1)
    | Load _ -> (0, 1)
    | OutputChar -> (1, 0)
    | OutputNumber -> (1, 0)
        
    | Clear _ -> failwith "Needs its own case"

/// Figures out the eventual fate for a value that
/// was just pushed onto the stack:
let eventualFate lastInstruction instructions =
    /// `st` = number of items on stack above the value.
    let rec eventualFateLocal st = function
        | [] ->
            match lastInstruction with
            | Exit -> Discarded // all values dropped on exit
            | Branch _ when st = 0 -> Consumed 
            | _ -> Pushed
        | Clear :: _ -> Discarded // all values dropped by clear
        | i :: is ->
            let (popped, pushed) = stackBehaviour i
            let st = st - popped
            if st < 0
            then
                if i = Discard
                then Discarded 
                else Consumed
            else eventualFateLocal (st + pushed) is

    eventualFateLocal 0 instructions

let rec performStackAnalysis
    (program : Parser.Program)
    ((_, initStack) : StateId)
    ((insns, last) : Chain<StateId>): TypedChain<StateId> =

    // acc = instruction acculuator
    // stk = stack
    // read = how many items read from stack
    let rec go acc read (stk : Stack) = function
        | [] ->
            let rec finale wrote = function
                | Push (KillDiscard, rest) ->
                    match last with
                    | Exit -> finale wrote rest // really this needs to check that all rest are killed
                    | _ -> failwith "KillDiscard left on stack"

                | Push (_, rest) -> finale (wrote+1) rest

                | EmptyStack ->
                    let (insns, lastInsn) =
                        if wrote = 0
                        // we know it's empty so we can propagate that information
                        // to the next chain.
                        then (List.rev acc, LastInstruction.map (fun (ip, _) -> (ip, EmptyStack)) last)
                        else (List.rev acc, LastInstruction.map (fun (ip, _) -> (ip, UnknownStack)) last)

                    //if read <> 0
                    //then failwith "Logic problem - claiming read values for known empty stack"

                    { instructions = insns
                    ; lastInstruction = lastInsn
                    ; stackBehaviour = (read, wrote)
                    }

                | UnknownStack ->
    
                    //match last with
                    //| Branch _ ->
                    //    if List.length stk <> 1
                    //    then failwith "Branch without push before it"
                    //| _ ->
                    //    if not (List.isEmpty stk)
                    //    then failwith "Function ended with non-empty local stack"

                    { instructions = List.rev acc
                    ; lastInstruction = last
                    ; stackBehaviour = (read, wrote)
                    }

            finale 0 stk
        | Load k :: is ->
            match eventualFate last is with
            | Discarded -> go acc read (push stk KillDiscard) is
            | _ -> go (Load k :: acc) read (push stk (Known k)) is

        | Discard :: is ->
            let (value, stk) = popAllowDiscard stk
            match value with
            | KillDiscard -> go acc read stk is
            | _ -> go (Discard :: acc) read stk is
            
        | (OutputChar as i) :: is
        | (OutputNumber as i) :: is ->
            let (value, stk) = pop stk
            match value with
            | Known v -> go (i :: Load v :: Discard :: acc) read stk is
            | _ -> go (i :: acc) read stk is

        | Clear :: is -> go (Clear :: acc) read EmptyStack is

        | BinOp op as bop :: is ->
        
            let (b, stk) = pop stk
            let (a, stk) = pop stk

            match (a, b) with
            | (Known a, Known b) ->
                let result =
                    match op with
                    | Multiply -> a * b
                    | Add -> a + b
                    | Subtract -> a - b
                    | Divide -> a / b
                    | Modulo -> a % b
                    | ReadText -> program.[b, a]
                    | Greater -> if a > b then 1 else 0
                go (Load result :: Discard :: Discard :: acc) read (push stk (Known result)) is

            | (_, Known 0)
            | (Known 0, _)
                when op = Multiply ->
                    go (Load 0 :: Discard :: Discard :: acc) read (push stk (Known 0)) is

            | (Known x, Known 1)
            | (Known 1, Known x)
                when op = Multiply ->
                    go (Load x :: Discard :: Discard :: acc) read (push stk (Known x)) is

            | (Known 0, Known x)
            | (Known x, Known 0)
                when op = Add ->
                    go (Load x :: Discard :: Discard :: acc) read (push stk (Known x)) is

            | _ -> go (bop :: acc) read (push stk Unknown) is

        | UnOp op as uop :: is ->
            let (value, stk) = pop stk
            match value with
            | Known x ->
                let result =
                    match op with 
                    | Not -> if x = 0 then 1 else 0

                go (Load result :: Discard :: acc) read (push stk (Known result)) is
            | _ -> go (uop :: acc) read (push stk Unknown) is
            
        | Dup :: is -> go (Dup :: acc) read (dup stk) is
        | Flip :: is ->
            let (a, stk) = pop stk
            let (b, stk) = pop stk
            match (a, b) with
            | (Known a', Known b') -> go (Load b' :: Load a' :: Discard :: Discard :: acc) read (push (push stk a) b) is
            | _ -> go (Flip :: acc) read (push (push stk a) b) is

        | (InputChar as i) :: is
        | (InputNumber as i) :: is -> go (i :: acc) read (push stk Unknown) is

    go [] 0 initStack insns

let optimizeLast fs last instructions =
    let noInstructions = List.isEmpty instructions

    match last with
    // identical branches - change to unconditional jump
    // we must add Discard to get rid of the existing Pop
    | Branch (l, r) when l = r -> (List.append instructions [Discard], toState r)

    // Rand with only a single distinct target must jump to it:
    | Rand [t] -> (instructions, toState t)

    // Rand that loops back to same function must eventually branch to another,
    // so if there are no instructions in this function we could just jump instead:
    | Rand [t1; t2] as r
        when noInstructions 
        -> 
            if fs = t1 
            then (instructions, toState t2)
            elif fs = t2
            then (instructions, toState t1)
            else (instructions, r)

    | c -> (instructions, c)

let optimizeChain program stateId (chain : TypedChain<StateId>) =
    fix peepholeOptimize chain.instructions
    |> optimizeLast stateId chain.lastInstruction
    |> performStackAnalysis program stateId
    |> fun x -> { x with instructions = fix peepholeOptimize x.instructions } // TODO why is this needed

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
        | Rand ts -> Seq.fold (fun m t -> copyIfNotExists t m) m ts
        | Exit -> m
        ) program program

let optimizeChains programText (program : TypedProgram) : TypedProgram =
    let newProgram = Map.map (optimizeChain programText) program
    instantiateSpecializations newProgram

// TODO: update to work with new types
let collapseIdenticalChains (m : Map<InstructionPointer.State, Instruction list * LastInstruction<InstructionPointer.State>>)
    : Map<InstructionPointer.State, Instruction list * LastInstruction<InstructionPointer.State>> =

    // invert the map, now all states with the same instruction list are in the same slot
    let inverted = invertMap m

    // pick one state to represent the others (the 'minimum')
    let newMappings : Map<InstructionPointer.State, InstructionPointer.State> =
        Map.fold (fun m _ fss ->
            let min = List.min fss in
            List.fold (fun m fs -> Map.add fs min m) m fss) Map.empty inverted

    // rewrite all the last-instructions to remap their states
    inverted
    |> Map.toSeq
    |> Seq.map (fun ((is, last), fss) ->
        let newLast =
            match last with
            | Exit -> exit
            | Branch (one, two) -> branch newMappings.[one] newMappings.[two]
            | Rand targets -> rand (Seq.map (fun x -> newMappings.[x]) targets)
            | ToState n -> toState (newMappings.[n])
        (List.min fss, (is, newLast)))
    |> Map.ofSeq

let optimize (options : Options) (programText : Parser.Program) (chains : Program<InstructionPointer.State>)
    : TypedProgram =

    // add initial types (stack behaviours) to the program
    let typedProgram =
        chains |> Map.fold (fun m ipState v ->
            let stack =
                if ipState = programEntryState
                then EmptyStack
                else UnknownStack

            let typedChain = performStackAnalysis programText (ipState, stack) (v |> (fun (c, last) -> (c, LastInstruction.map (fun i -> (i, UnknownStack)) last)))

            if options.verbose then printfn "%A has stack behaviour %A" (ipState, stack) typedChain.stackBehaviour

            Map.add (ipState, stack) typedChain m)
            Map.empty

    // optimize until we run out of optimizations
    instantiateSpecializations typedProgram
    |> fixN (fun n x ->
        if options.verbose then printfn "\nPerforming optimization pass %d" n
        optimizeChains programText (inlineChains options x))
