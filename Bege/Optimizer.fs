module Bege.Optimizer

open System

open Bege.AST
open Bege.Common
open Bege.InstructionPointer


type Stack
    = UnknownStack of int // how many values have we popped from this
    | EmptyStack
    | Value of int option * Stack

type LocalStack = int option list

type StateId = IPState * Stack

let inlineChains (program : Program<StateId>) : Program<StateId> =

    let inc k m =
        match Map.tryFind k m with
        | None -> Map.add k 1 m
        | Some c -> Map.add k (c+1) m

    let countReferences m =
            m |> Map.fold (fun cs _ (_, li) -> 
                match li with
                | Exit -> cs
                | Branch (l, r) -> inc l (inc r cs)
                | Rand targets -> List.fold (fun cs x -> inc x cs) cs targets
                | ToState n -> inc n cs) Map.empty
     
    let counts = countReferences program

    let canBeInlined fs =
        counts.[fs] = 1
    
    let isJump fs =
        match program.[fs] with
        | ([], ToState t) -> true
        | _ -> false
    
    let target fs = 
        match program.[fs] with 
        | ([], ToState t) -> t
        | _ -> failwith "target must be guarded by isJump"

    let result =
        program
        |> Map.map (fun fs ((il, li) as value) ->
            match li with
            | ToState n when canBeInlined n || isJump n ->
                // printfn "Inlining %A into %A" n fs
                let il', li' = program.[n] in
                (List.append il il', li')
            | Branch (l, r) when isJump l || isJump r ->
                let l = if isJump l then target l else l
                let r = if isJump r then target r else r
                (il, Branch (l, r))
            | Rand targets
                when List.exists isJump targets ->
                let newTargets = List.map (fun x -> if isJump x then target x else x) targets
                (il, Rand newTargets)
            | _ ->  value)

    let newCounts = countReferences result

    result
    |> Map.filter (fun ((ip, s) as k) _ -> newCounts.ContainsKey k || (ip = programEntryState && s = EmptyStack))
    (* can't remove entry state *)

let rec peepholeOptimize (prog : Parser.Program) = function
    // Unneeded push/pops
    | Push :: Pop :: is -> peepholeOptimize prog is
    | Dup :: Pop :: is -> peepholeOptimize prog is
    | Load _ :: Discard :: is -> peepholeOptimize prog is
    // constant folding
    | Load b :: Push :: Load a :: Pop :: BinOp k :: is ->
        let apply a b =
            match k with
            | Add -> b + a
            | Subtract -> b - a
            | Multiply -> b * a
            | Divide -> b / a
            | Modulo -> b % a
            | Greater -> if b > a then 1 else 0
            | ReadText -> prog.[a, b]

        peepholeOptimize prog (Load (apply a b) :: is)
    //| Push x :: Push y :: Add :: is -> peepholeOptimize (Push (x + y) :: is)
    //| Push x :: Push y :: Divide :: is -> peepholeOptimize (Push (x / y) :: is)
    //| Push x :: Push y :: Multiply :: is -> peepholeOptimize (Push (x * y) :: is)
    //| Push x :: Push y :: Subtract :: is -> peepholeOptimize (Push (x - y) :: is)
    //| Push x :: Push y :: Greater :: is -> peepholeOptimize (Push (Convert.ToInt32(x > y)) :: is)
    //| Push x :: Not :: is -> peepholeOptimize (Push (Convert.ToInt32((x = 0))) :: is)
    // eliminate unneeded nots
    | UnOp Not :: UnOp Not :: is -> peepholeOptimize prog is
    // eliminate unneeded flips
    //| Push :: Push :: Pop :: Pop :: Flip :: is -> peepholeOptimize (Push y :: Push x :: is)
    | Flip :: Flip :: is -> peepholeOptimize prog is
    | Dup :: Flip :: is -> Dup :: peepholeOptimize prog is
    // eliminate dead pushes
    | (i :: is) -> i :: peepholeOptimize prog is
    | [] -> []

let rec performStackAnalysis (program : Parser.Program) (ip, initStack) (insns, last) = 

    // go : GlobalStack -> LocalStack -> Instruction list
    let rec go gs ls = function
        | [] ->
            match gs with
            | EmptyStack ->
                // we know it's empty so we can propagate that information
                // to the next chain
                (insns, LastInstruction.map (fun (ip, st) -> (ip, EmptyStack)) last)

            | _ ->
                match last with
                | Branch _ ->
                    if List.length ls <> 1
                    then failwith "Branch without push before it"
                | _ ->
                    if not (List.isEmpty ls)
                    then failwith "Function ended with non-empty local stack"

                (insns, last)

        | Load k :: is -> go gs (Some k :: ls) is
        | Push :: is ->
            match ls with
            | (l :: ls) -> go (Value (l, gs)) ls is
            | [] -> failwith "Pushed from empty local stack"
        | Pop :: is ->
            match gs with
            | EmptyStack -> go EmptyStack (Some 0 :: ls) is
            | UnknownStack pops -> go (UnknownStack (pops+1)) (None :: ls) is
            | Value (v, rest) -> go rest (v :: ls) is
        | Discard :: is
        | OutputChar :: is
        | OutputNumber :: is ->
            match ls with
            | (_ :: ls) -> go gs ls is
            | [] -> failwith "Unexpected empty stack"
        | Clear :: is ->
            match ls with
            | [] -> go EmptyStack ls is
            | _ -> failwith "Clear with unclear local stack"
        | BinOp op :: is ->

            match ls with
            | Some a :: Some b :: ls -> 
                let result = 
                    match op with
                    | Multiply -> b * a
                    | Add -> b + a
                    | Subtract -> b - a
                    | Divide -> b / a
                    | Modulo -> b % a
                    | ReadText -> program.[a, b]
                    | Greater -> if b > a then 1 else 0
                go gs (Some result :: ls) is

            | Some 0 :: x :: ls
            | x :: Some 0 :: ls ->
                match op with
                | Multiply -> go gs (Some 0 :: ls) is
                | Add -> go gs (x :: ls) is
                | _ -> go gs (None :: ls) is

            | Some 1 :: x :: ls
            | x :: Some 1 :: ls ->
                match op with
                | Multiply -> go gs (x :: ls) is
                | _ -> go gs (None :: ls) is
            
            | _ :: _ :: ls -> go gs (None :: ls) is // unknown result
            | _ -> failwith "Binary operation without enough arguments on stack"

        | UnOp op :: is ->
            match ls with
            | Some x :: ls ->
                let result =
                    match op with
                    | Not -> if x = 0 then 1 else 0
                
                go gs (Some result :: ls) is

            | _ :: ls -> go gs (None :: ls) is // unknown result
            | _ -> failwith "Unary operation without enough arguments on stack"

        | Dup :: is ->
            match ls with
            | (v :: _) -> go gs (v :: ls) is
            | [] -> failwith "Dup with empty stack"
        | Flip :: is ->
            match ls with
            | (x :: y :: ls) -> go gs (y :: x :: ls) is
            | _ -> failwith "Flip without enough arguments on stack"
        | InputChar :: is -> go gs (None :: ls) is
        | InputNumber :: is -> go gs (None :: ls) is

    go initStack [] insns

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

let optimizeChain program stateId (insns, last) = 
    fix (peepholeOptimize program) insns
    |> optimizeLast stateId last
    |> performStackAnalysis program stateId

let optimizeChains programText program  =
    let newProgram = Map.map (optimizeChain programText) program

    // it could be the case that performStackAnalysis added references
    // to specialized chains that don't exist, so we will instantiate them:

    // copy the existing one with "default" stack if it doesn't already exist
    let copyIfNotExists ((ip, _) as state) m =
        if not (Map.containsKey state m)
        then Map.add state (m.[(ip, UnknownStack 0)]) m
        else m

    Map.fold (fun m k (_, last) -> 
        match last with
        | ToState t -> copyIfNotExists t m
        | Branch (z, nz) -> copyIfNotExists z (copyIfNotExists nz m)
        | Rand ts -> List.fold (fun m t -> copyIfNotExists t m) m ts
        | Exit -> m
        ) newProgram newProgram

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
    (is, LastInstruction.map (fun ipState -> (ipState, UnknownStack 0)) last)

let optimize (program : Parser.Program) (chains : Program<IPState>)
    : Program<StateId> = 

    let chainsWithStacks = 
        Map.fold (fun m k v ->
            let newKey =
                if k = programEntryState
                then (k, EmptyStack)
                else (k, UnknownStack 0)
            Map.add newKey (augmentChain v) m) Map.empty chains

    fix (optimizeChains program << inlineChains) chainsWithStacks
    