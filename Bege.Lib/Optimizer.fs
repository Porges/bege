module Bege.Optimizer

open Bege.Common
open Bege.InstructionPointer
open Bege.Parser
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

module Stack =
    type Stack
        = Forever of StackValue
        | Push of StackValue * Stack

    let emptyStack = Forever (Known 0)
    let unknownStack = Forever (Unknown)
        
    let push stk value =
        match (stk, value) with
        //| (Forever x, y) when x = y -> Forever x
        | (stk, value) -> Push (value, stk)

    let pop = function
        | Forever x -> (x, Forever x)
        | Push (KillDiscard, _) -> failwith "Unexpected KillDiscard"
        | Push (value, rest) -> (value, rest)
        
    let popAllowDiscard = function
        | Forever x -> (x, Forever x)
        | Push (value, rest) -> (value, rest)
        
    let dup stk = 
        let (value, stk) = pop stk
        push (push stk value) value

    let rec simplify = function 
        | Forever _ as stk -> stk
        | Push (x, stk) ->
            match simplify stk with
            | Forever y as stk when x = y -> stk
            | stk -> Push (x, stk)

module TrackedStack =
    type TrackedStack = { read: int; stack: Stack.Stack }

    let emptyStack = { read = 0; stack = Stack.emptyStack }
    let unknownStack = { read = 0; stack = Stack.unknownStack }

    let push stk value = { stk with stack = Stack.push stk.stack value }
    let pop stk = 
        let (x, newStack) = Stack.pop stk.stack
        match stk.stack with
        | Stack.Forever _ ->
            (x, { read = stk.read + 1; stack = newStack })
        | _ -> 
            (x, { stk with stack = newStack })

    let popAllowDiscard stk = 
        let (x, newStack) = Stack.popAllowDiscard stk.stack
        match stk.stack with
        | Stack.Forever _ -> 
            (x, { read = stk.read + 1; stack = newStack })
        | _ ->
            (x, { stk with stack = newStack })

    let dup stk =
        let (value, stk) = pop stk
        push (push stk value) value

/// How many items the chain pops, and how many it stores.
type StackBehaviour = int * int
type StateId = InstructionPointer.State * Stack.Stack

type TypedChain = { chain: Chain<StateId>; behaviour: StackBehaviour }

type TypedProgram = Map<StateId, TypedChain>

let inlineChains (options : Options) (program : TypedProgram) : TypedProgram =

    let inc k m =
        match Map.tryFind k m with
        | None -> Map.add k 1 m
        | Some c -> Map.add k (c+1) m

    let countReferences m =
            m |> Map.fold (fun cs _ c ->
                match c.chain.control with
                | Exit -> cs
                | Branch (l, r) -> inc l (inc r cs)
                | Rand targets -> Seq.fold (fun cs x -> inc x cs) cs targets
                | ToState n -> inc n cs) Map.empty

    let counts = countReferences program

    let canBeInlined fs =
        counts.[fs] = 1

    let isJump start =
        match program.[start] with
        | { chain = { instructions = []; control = ToState _ } } -> true
        | _ -> false

    let target fs =
        match program.[fs] with
        | { chain = { instructions = []; control = ToState t } } -> t
        | _ -> failwith "target must be guarded by isJump"

    let result =
        program
        |> Map.map (fun _ c ->
            match c.chain.control with
            | ToState n
                when canBeInlined n || isJump n ->
                fprintf options.verbose "Inlining %A into %A" n c.chain.start
                let targetChain = program.[n]
                { chain =
                    { start = c.chain.start
                    ; instructions = (List.append c.chain.instructions targetChain.chain.instructions)
                    ; control = targetChain.chain.control }
                ; behaviour = c.behaviour // TODO: this is wrong!
                }
            | Branch (z, nz)
                when isJump z || isJump nz ->
                let z = if isJump z then target z else z
                let nz = if isJump nz then target nz else nz
                { c with chain = { c.chain with control = Branch z nz } }
            | Rand targets
                when targets.Any(fun x -> isJump x) ->
                let newTargets = Seq.map (fun x -> if isJump x then target x else x) targets
                { c with chain = { c.chain with control = Rand newTargets } }
            | _ ->  c)

    let newCounts = countReferences result

    let trimmedResult =
        result
        (* can't remove entry state *)
        |> Map.filter (fun ((ip, s) as k) _ -> newCounts.ContainsKey k || (ip = programEntryState && s = Stack.emptyStack))

    fprintfn options.verbose "Inlining reduced chains from %d to %d." program.Count trimmedResult.Count

    trimmedResult

let peepholeOptimize (c: Chain<_>): Chain<_> =
    let rec go = function
        // dead loads
        | Load _ :: Discard :: is -> go is
        // dead computations
        | UnOp _ :: Discard :: is -> Discard :: go is
        | BinOp _ :: Discard :: is -> Discard :: Discard :: go is
        // unneeded discards
        | Discard :: Clear :: is -> Clear :: go is
        | Clear :: Clear :: is -> Clear :: go is
        // eliminate unneeded nots
        | UnOp Not :: UnOp Not :: is -> go is
        // eliminate unneeded flips
        | Flip :: Flip :: is -> go is
        | Dup :: Flip :: is -> Dup :: go is
        // eliminated unneeded dups
        | Dup :: Discard :: is -> go is

        | i :: is -> i :: go is
        | [] -> []

    { c with instructions = go c.instructions }

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
        
    | Clear _ -> failwith "Clear needs its own case"

/// Figures out the eventual fate for a value that
/// was just pushed onto the stack:
let eventualFate (c: Chain<_>): EventualFate =
    /// `st` = number of items on stack above the value.
    let rec eventualFateLocal st = function
        | [] ->
            match c.control with
            | Exit -> Discarded // all values dropped on exit
            | Branch _ when st = 0 -> Consumed
            | _ -> Pushed
        | Clear :: _ -> Discarded // all values dropped by clear
        | i :: is ->
            let (popped, pushed) = stackBehaviour i
            let st = st - popped
            if st < 0
            then
                if i = Discard then Discarded else Consumed
            else
                eventualFateLocal (st + pushed) is

    eventualFateLocal 0 c.instructions

open TrackedStack

let performStackAnalysis
    (memory: BefungeSpace)
    (initStack: Stack.Stack)
    ({instructions=instructions; control=control; start=start} : Chain)
    : TypedChain =

    // acc = instruction acculuator
    // stk = stack
    // read = how many items read from stack
    let rec go acc (stk : TrackedStack) = function
        | [] ->
            let rec finale wrote = function
                | Stack.Push (KillDiscard, rest) ->
                    match control with
                    | Exit -> finale wrote rest // TODO: really this needs to check that all values on stack are killed
                    | _ -> failwith "KillDiscard left on stack"

                | Stack.Push (_, rest) -> finale (wrote+1) rest

                | x when x = Stack.emptyStack ->
                    let control =
                        if wrote = 0
                        // we know it's empty so we can propagate that information
                        // to the next chain.
                        then Control.map (fun ip -> (ip, Stack.emptyStack)) control
                        else Control.map (fun ip -> (ip, Stack.unknownStack)) control

                    { chain = { start = (start, initStack); instructions = List.rev acc; control = control }
                    ; behaviour = (stk.read, wrote)
                    }

                | x when x = Stack.unknownStack ->

                    let control = Control.map (fun ip -> (ip, Stack.unknownStack)) control
    
                    { chain = { start = (start, initStack); instructions = List.rev acc; control = control }
                    ; behaviour = (stk.read, wrote)
                    }

                | Stack.Forever _ -> failwith "Invalid stack created" // Forever should only be Known 0 or Unknown

            finale 0 stk.stack
        | Load k :: is ->
            match eventualFate { start = start; instructions = is; control = control} with
            | Discarded -> go acc (push stk KillDiscard) is
            | _ -> go (Load k :: acc) (push stk (Known k)) is

        | Discard :: is ->
            let (value, stk) = popAllowDiscard stk
            match value with
            | KillDiscard -> go acc stk is
            | _ -> go (Discard :: acc) stk is
        
        | (OutputChar as i) :: is
        | (OutputNumber as i) :: is ->
            let (value, stk) = pop stk
            match value with
            | Known v -> go (i :: Load v :: Discard :: acc) stk is
            | _ -> go (i :: acc) stk is

        | Clear :: is -> go (Clear :: acc) emptyStack is

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
                    | ReadText -> memory.[b, a]
                    | Greater -> if a > b then 1 else 0
                go (Load result :: Discard :: Discard :: acc) (push stk (Known result)) is

            | (_, Known 0)
            | (Known 0, _)
                when op = Multiply ->
                    go (Load 0 :: Discard :: Discard :: acc) (push stk (Known 0)) is

            | (Known x, Known 1)
            | (Known 1, Known x)
                when op = Multiply ->
                    go (Load x :: Discard :: Discard :: acc) (push stk (Known x)) is

            | (Known 0, Known x)
            | (Known x, Known 0)
                when op = Add ->
                    go (Load x :: Discard :: Discard :: acc) (push stk (Known x)) is

            | _ -> go (bop :: acc) (push stk Unknown) is

        | UnOp op as uop :: is ->
            let (value, stk) = pop stk
            match value with
            | Known x ->
                let result =
                    match op with 
                    | Not -> if x = 0 then 1 else 0

                go (Load result :: Discard :: acc) (push stk (Known result)) is
            | _ -> go (uop :: acc) (push stk Unknown) is
            
        | Dup :: is -> go (Dup :: acc) (dup stk) is
        | Flip :: is ->
            let (a, stk) = pop stk
            let (b, stk) = pop stk
            match (a, b) with
            | (Known a', Known b') -> go (Load b' :: Load a' :: Discard :: Discard :: acc) (push (push stk a) b) is
            | _ -> go (Flip :: acc) (push (push stk a) b) is

        | (InputChar as i) :: is
        | (InputNumber as i) :: is -> go (i :: acc) (push stk Unknown) is

    go [] { read = 0; stack = initStack } instructions

let optimizeControl (tc: TypedChain): TypedChain =

    match tc.chain.control with

    // identical branches - change to unconditional jump
    // we must add Discard to have the same stack behaviour
    | Branch (l, r) when l = r ->
        { tc with 
            chain = 
            { tc.chain with
                  instructions = List.append tc.chain.instructions [Discard]
                  ; control = ToState r }
        }

    // Rand with only a single distinct target must jump to it:
    | Rand [t] ->
        { tc with 
            chain = { tc.chain with control = ToState t }
        }

    // Rand that loops back to same function must eventually branch to another,
    // so if there are no instructions in this function we could just jump instead:
    | Rand [t1; t2] as r
        when List.isEmpty tc.chain.instructions 
        -> 
            if tc.chain.start = t1 
            then { tc with chain = { tc.chain with control = ToState t2 } }
            elif tc.chain.start = t2
            then { tc with chain = { tc.chain with control = ToState t1 } }
            else tc 

    | _ -> tc

let optimizeChain (options: Options) (memory: BefungeSpace) (tc : TypedChain): TypedChain =

    let before = tc

    let after =
        tc
        |> optimizeControl
        |> fun tc -> { tc with chain = fix peepholeOptimize tc.chain }

    if before <> after
    then fprintfn options.verbose "Chain optimized from:\n    %A\nto:\n    %A" before.chain after.chain

    after

    
/// performStackAnalysis can add references to "specializations"
/// that don't exist yet. This function creates them.
let instantiateSpecializations (program: TypedProgram): TypedProgram =
    // copy the existing one with "default" stack if it doesn't already exist
    let copyIfNotExists ((ip, _) as state) m =
        if not (Map.containsKey state m)
        then Map.add state m.[(ip, Stack.unknownStack)] m
        else m

    Map.fold (fun m _ c ->
        match c.chain.control with
        | ToState t -> copyIfNotExists t m
        | Branch (z, nz) -> copyIfNotExists z (copyIfNotExists nz m)
        | Rand ts -> Seq.fold (fun m t -> copyIfNotExists t m) m ts
        | Exit -> m
        ) program program

let optimizeChains options memory (program : TypedProgram) : TypedProgram =
    program
    |> Map.map (fun _ v -> optimizeChain options memory v)
    |> instantiateSpecializations

let collapseIdenticalChains (m : Map<_, TypedChain>): Map<_, TypedChain> =

    let lookup =
        m
        |> Map.toSeq
        |> Seq.groupBy (fun (_, v) -> v.chain) // group same chains together
        |> Seq.map (fun (_, vs) -> // associate states with 'min' of states with same chain
            let states = Seq.map fst vs |> List.ofSeq
            let min = List.min states
            states |> Seq.map (fun s -> (s, min)))
        |> Seq.concat
        |> Map.ofSeq
    
    // drop all that aren't in the set
    let filtered = Map.fold (fun m key _ -> if lookup.[key] = key then m else Map.remove key m) m m

    // rewrite all control instructions
    let result =
        filtered
        |> Map.map (fun _ tc ->
            { tc with chain = { tc.chain with control = Control.map (fun t -> lookup.[t]) tc.chain.control } })

    printfn "Reduced from %d to %d chains" m.Count result.Count

    result

// add initial types (stack behaviours) to the program
//let toTypedProgram (options: Options) (program: Program<_>): TypedProgram =
//    program.chains
//    |> Map.fold (fun m ipState v ->
//        let stack =
//            if ipState = programEntryState
//            then emptyStack
//            else unknownStack

//        let typedChain = performStackAnalysis program.memory (toTypedChain v)
        
//        fprintfn options.verbose "%A has stack behaviour %A" (ipState, stack) typedChain.behaviour

//        Map.add (ipState, stack) typedChain m)
//        Map.empty

//let optimize (options : Options) (program: Program<_>): TypedProgram =

//    // optimize until we run out of optimizations
//    program
//    |> toTypedProgram options
//    |> instantiateSpecializations
//    |> fixN (fun n x ->
//        fprintfn options.verbose "--- Performing optimization pass %d ---" n
//        x
//        |> inlineChains options 
//        |> optimizeChains options program.memory
//        |> collapseIdenticalChains)

let optimize (options: Options) (memory: BefungeSpace) (initStack: Stack.Stack) (chain: Chain): TypedChain =
    performStackAnalysis memory initStack chain
    |> optimizeChain options memory

