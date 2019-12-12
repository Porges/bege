module Bege.Renderer

open System
open System.IO
open System.Reflection
open System.Reflection.Emit

open Bege.Parser
open Bege.Optimizer
open Bege.Options

type private ExplicitStack =
    | Push
    | Pop
    | I of Instruction
    | C of Control<int>

let private makeExplicit (chain: Chain<int>): ExplicitStack list = 

    let explicitInstruction = function
        | Clear -> Seq.singleton (I Clear)
        | instruction ->
            let (pop, push) = stackBehaviour instruction
            Seq.concat [ Seq.replicate pop Pop; Seq.singleton (I instruction); Seq.replicate push Push ]

    let explicitControl = function
        | Branch _ as b -> seq { Pop; C b }
        // no other control pops
        | c -> seq { C c }

    chain.instructions
    |> Seq.map explicitInstruction
    |> Seq.concat
    |> Bege.Common.flip Seq.append (explicitControl chain.control)
    |> List.ofSeq

let private optimizeExplicit (options: Options) (instructions: ExplicitStack list) =
    if not options.optimize
    then instructions
    else
        let rec go = function
            | Push :: (I x) :: Pop :: is
                when stackBehaviour x = (0, 1) -> I x :: I Flip :: go is
            | Push :: Pop :: is -> go is
            | i :: is -> i :: go is
            | [] -> []

        fprintfn options.verbose "Explicit form:\n\t%A" instructions

        let after = Bege.Common.fix go instructions
        if instructions <> after then fprintfn options.verbose "Optimized explicit to:\n\t%A" after
        after

type FungeFace =
    abstract Pop : unit -> int 
    abstract Push : int -> unit
    abstract Clear : unit -> unit
    abstract OutputChar : int -> unit
    abstract OutputNumber : int -> unit
    abstract InputChar : unit -> int
    abstract InputNumber : unit -> int
    abstract ReadText : int * int -> int
    abstract Rand : unit -> int
    abstract Call : int -> unit

module private Methods =
   let private m n = typeof<FungeFace>.GetMethod(n, BindingFlags.Instance ||| BindingFlags.Public)
   let push = m "Push"
   let pop = m "Pop"
   let clear = m "Clear"
   let outputChar = m "OutputChar"
   let inputChar = m "InputChar"
   let inputNumber = m "InputNumber"
   let outputNumber = m "OutputNumber"
   let readText = m "ReadText"
   let rand = m "Rand"
   let call = m "Call"

let private emitChain (options: Options) (il: ILGenerator) (chain: Chain<int>): unit =
    fprintfn options.verbose "--- Emitting method: %d" chain.start

    // might need up to 2 locals
    il.DeclareLocal(typeof<int>) |> ignore
    il.DeclareLocal(typeof<int>) |> ignore

    let callHelper (mi : MethodInfo) =
        let args = mi.GetParameters().Length
        if args = 0 then
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, mi)
        elif args = 1 then
            il.Emit(OpCodes.Stloc_0)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldloc_0)
            il.Emit(OpCodes.Callvirt, mi)
        elif args = 2 then
            il.Emit(OpCodes.Stloc_1)
            il.Emit(OpCodes.Stloc_0)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldloc_0)
            il.Emit(OpCodes.Ldloc_1)
            il.Emit(OpCodes.Callvirt, mi)
        else
            raise <| new NotSupportedException()

    let emitLoadInt = function
        | -1 -> il.Emit(OpCodes.Ldc_I4_M1)
        | 0 -> il.Emit(OpCodes.Ldc_I4_0)
        | 1 -> il.Emit(OpCodes.Ldc_I4_1)
        | 2 -> il.Emit(OpCodes.Ldc_I4_2)
        | 3 -> il.Emit(OpCodes.Ldc_I4_3)
        | 4 -> il.Emit(OpCodes.Ldc_I4_4)
        | 5 -> il.Emit(OpCodes.Ldc_I4_5)
        | 6 -> il.Emit(OpCodes.Ldc_I4_6)
        | 7 -> il.Emit(OpCodes.Ldc_I4_7)
        | 8 -> il.Emit(OpCodes.Ldc_I4_8)
        | v -> il.Emit(OpCodes.Ldc_I4, v)

    let tailTo (next: int) =
        il.Emit(OpCodes.Ldarg_0)
        emitLoadInt next
        il.Emit(OpCodes.Tailcall)
        il.Emit(OpCodes.Callvirt, Methods.call)
        il.Emit(OpCodes.Ret)

    let emitFlip () =
        il.Emit(OpCodes.Stloc_0)
        il.Emit(OpCodes.Stloc_1)
        il.Emit(OpCodes.Ldloc_0)
        il.Emit(OpCodes.Ldloc_1)

    let emitInstruction = function
        | Load value ->
            emitLoadInt value
        | Discard ->
            il.Emit(OpCodes.Pop)
        | Clear ->
            callHelper Methods.clear
        | Flip ->
            emitFlip()
        | Dup ->
            il.Emit(OpCodes.Dup)
        | InputChar ->
            callHelper Methods.inputChar
        | InputNumber ->
            callHelper Methods.inputNumber
        | OutputChar ->
            callHelper Methods.outputChar
        | OutputNumber ->
            callHelper Methods.outputNumber
        | BinOp op ->
            match op with
            | Greater ->
                // we use "less than" rather than flipping and
                // calling greater than
                il.Emit(OpCodes.Clt)
            | Add ->
                il.Emit(OpCodes.Add) // order doesn't matter
            | Multiply ->
                il.Emit(OpCodes.Mul) // order doesn't matter
            | Divide ->
                emitFlip()
                il.Emit(OpCodes.Div)
            | Modulo ->
                emitFlip()
                il.Emit(OpCodes.Rem)
            | Subtract ->
                emitFlip()
                il.Emit(OpCodes.Sub)
            | ReadText ->
                emitFlip()
                callHelper Methods.readText

        | UnOp op ->
            match op with
            | Not ->
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ceq)

    let emitControl = function
        | Rand targets ->
            let methods =
                targets
                |> Seq.map (fun t -> (il.DefineLabel(), t))
                |> Seq.toArray
                                         
            callHelper Methods.rand

            il.Emit(OpCodes.Switch, methods |> Array.map (fun (lbl, _) -> lbl))

            methods |> Array.iter (fun (lbl, target) -> il.MarkLabel lbl; tailTo target)

        | Branch (zero, nonZero) ->
            let zeroTarget = il.DefineLabel()
            
            il.Emit(OpCodes.Brfalse, zeroTarget)
            tailTo nonZero
            il.MarkLabel(zeroTarget)
            tailTo zero

        | Exit ->
            il.Emit(OpCodes.Ret)

        | ToState next ->
            tailTo next

    let emit = function
        | Push -> callHelper Methods.push
        | Pop -> callHelper Methods.pop
        | I insn -> emitInstruction insn
        | C control -> emitControl control

    chain
    |> makeExplicit
    |> optimizeExplicit options
    |> Seq.iter emit


let private fungeFaceArg = [| typeof<FungeFace> |]

let defineMethod (options: Options) (chain: Chain<int>): Action<FungeFace> =
    
    let method = DynamicMethod (string(chain.start), typeof<Void>, fungeFaceArg)

    let il = method.GetILGenerator()
    emitChain options il chain

    method.CreateDelegate(typeof<Action<FungeFace>>) :?> Action<FungeFace>

