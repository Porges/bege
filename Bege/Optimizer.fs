module Bege.Optimizer

open System

open Bege.AST
open Bege.Common
open Bege.InstructionPointer

let inlineChains (m : Map<IPState, Instruction list * LastInstruction>) =

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
     
    let counts = countReferences m

    let canBeInlined fs =
        counts.[fs] = 1
    
    let isJump fs =
        match m.[fs] with
        | ([], ToState t) -> true
        | _ -> false
    
    let target fs = 
        match m.[fs] with // must be guarded by `isJump`
        | ([], ToState t) -> t

    let result =
        m
        |> Map.map (fun fs ((il, li) as value) ->
            match li with
            | ToState n when canBeInlined n || isJump n ->
                // printfn "Inlining %A into %A" n fs
                let il', li' = m.[n] in
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

    result |> Map.filter (fun k _ -> newCounts.ContainsKey k || k = programEntryState (* can't remove entry state *))

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

type Stack
    = UnknownStack of int // how many values have we popped from this
    | EmptyStack
    | Value of int option * Stack

type LocalStack = int option list

let rec performStackAnalysis ip (insns, last) = 

    // go : GlobalStack -> LocalStack -> Instruction list
    let rec go gs ls = function
        | [] -> []
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
        | Discard :: is | OutputChar :: is | OutputNumber :: is ->
            match ls with
            | (_ :: ls) -> go gs ls is
            | [] -> failwith "Unexpected empty stack"
        | BinOp op :: is ->
            match ls with
            | (Some a :: Some b :: ls) -> 
                let result = 
                    match op with
                    | Multiply -> b * a
                    | Add -> b + a
                    | Subtract -> b - a
                    | Divide -> b / a
                    | Greater -> if b > a then 1 else 0
                go gs (Some result :: ls) is
            | _ :: _ :: ls -> go gs (None :: ls) is // unknown result
            | _ -> failwith "Binary operation without enough arguments on stack"
        | UnOp op :: is ->
            match ls with
            | Some a :: ls ->
                let result =
                    match op with
                    | Not -> if a = 0 then 1 else 0
                
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

    go (UnknownStack 0) [] insns

let optimizeLast fs instructions last =
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

let optimizeChain program ipState (insns, last) = optimizeLast ipState (fix (peepholeOptimize program) insns) last
let optimizeChains program = Map.map (optimizeChain program)

let collapseIdenticalChains (m : Map<IPState, Instruction list * LastInstruction>) : Map<IPState, Instruction list * LastInstruction> =

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

let optimize (program : Parser.Program) (chains : Map<IPState, Instruction list * LastInstruction>)
    : Map<IPState, Instruction list * LastInstruction> = 

    fix (optimizeChains program << inlineChains << collapseIdenticalChains) chains