module Bege.Renderer

open System
open System.IO
open System.Reflection
open System.Reflection.Emit

open Bege.Parser
open Bege.Optimizer
open Bege.Options
open Bege.Runtime

let assemblyName = AssemblyName "BefungeAssembly"
let moduleName = "BefungeModule"
let programClassName = "BefungeProgram"

let defineRunMethod (tb : TypeBuilder) (initialFunction : string) (definedMethods : Map<string, MethodBuilder * TypedChain<string>>) : unit =
    let runMethod = tb.DefineMethod("Run", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<int>, Type.EmptyTypes)
    let (entryMethod, _) = definedMethods.[initialFunction]
    let runIL = runMethod.GetILGenerator()
    runIL.Emit(OpCodes.Ldarg_0)
    runIL.Emit(OpCodes.Call, entryMethod)
    runIL.Emit(OpCodes.Ldarg_0)
    runIL.Emit(OpCodes.Call, BaseMethods.count)
    runIL.Emit(OpCodes.Ret)

let defineConstructor (tb : TypeBuilder) (progText : string) : unit =
    let ctor =
        tb.DefineMethod(
            ".ctor",
            MethodAttributes.Public ||| MethodAttributes.SpecialName,
            typeof<Void>,
            [| typeof<TextReader>; typeof<TextWriter>; typeof<uint64> |])

    let il = ctor.GetILGenerator()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Ldarg_1)
    il.Emit(OpCodes.Ldarg_2)
    il.Emit(OpCodes.Ldstr, progText)
    il.Emit(OpCodes.Ldarg_3)
    il.Emit(OpCodes.Call, BaseMethods.ctor)
    il.Emit(OpCodes.Ret)

    let emptyCtor =
        tb.DefineMethod(".ctor", MethodAttributes.Public ||| MethodAttributes.SpecialName)

    let emptyIL = emptyCtor.GetILGenerator()
    emptyIL.Emit(OpCodes.Ldarg_0)
    emptyIL.Emit(OpCodes.Ldstr, progText)
    emptyIL.Emit(OpCodes.Call, BaseMethods.easyCtor)
    emptyIL.Emit(OpCodes.Ret)

let defineMain (dynMod : ModuleBuilder) (programType : Type) : MethodInfo =
    let entryPoint = dynMod.DefineType("EntryPoint", TypeAttributes.Sealed ||| TypeAttributes.BeforeFieldInit, typeof<obj>)
    let methodAttributes =
        MethodAttributes.Static |||
        MethodAttributes.Public |||
        MethodAttributes.HideBySig

    let main = entryPoint.DefineMethod("Main", methodAttributes, typeof<int>, [| typeof<string[]> |])
    let il = main.GetILGenerator()
    il.Emit(OpCodes.Newobj, programType.GetConstructor(Type.EmptyTypes))
    il.Emit(OpCodes.Call, programType.GetMethod("Run"))
    il.Emit(OpCodes.Ret)

    entryPoint.CreateType() |> ignore
    entryPoint.GetMethod("Main")

let buildType (options : Options) (progText : string) (chains : Map<string, TypedChain<string>>, initialFunction : string) : Type =

    let dynAsm = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndCollect)

    let dynMod = dynAsm.DefineDynamicModule moduleName
    let typeAttributes =
        TypeAttributes.Public |||
        TypeAttributes.Class |||
        TypeAttributes.Sealed |||
        TypeAttributes.AutoLayout

    let tb = dynMod.DefineType(programClassName, typeAttributes, typeof<Runtime.Funge>)

    let definedMethods =
        chains |> Map.map (fun name chain ->
            let args = fst chain.behaviour
            fprintfn options.verbose "Defining %s (%d args)" name args
            let args = Array.replicate 0 (* TODO: args *) typeof<int>
            let method = tb.DefineMethod(name, MethodAttributes.Private, typeof<Void>, args)
            (method, chain))

    let buildMethod (method : MethodBuilder, tc: TypedChain<_>) =
        // printfn "Emitting method: %s" (nameFromState fs)
        let il = method.GetILGenerator()
        let l1 = il.DeclareLocal(typeof<int>)
        let l2 = il.DeclareLocal(typeof<int>)

        let callBase (mi : MethodInfo) =
            let args = mi.GetParameters().Length
            if args = 0 then
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Call, mi)
            elif args = 1 then
                il.Emit(OpCodes.Stloc_0)
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldloc_0)
                il.Emit(OpCodes.Call, mi)
            elif args = 2 then
                il.Emit(OpCodes.Stloc_1)
                il.Emit(OpCodes.Stloc_0)
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldloc_0)
                il.Emit(OpCodes.Ldloc_1)
                il.Emit(OpCodes.Call, mi)
            else
                raise <| new NotSupportedException()

        let tailTo (mi : MethodInfo) =
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Tailcall)
            il.Emit(OpCodes.Call, mi)
            il.Emit(OpCodes.Ret)

        let emitFlip() =
            il.Emit(OpCodes.Stloc_0)
            il.Emit(OpCodes.Stloc_1)
            il.Emit(OpCodes.Ldloc_0)
            il.Emit(OpCodes.Ldloc_1)

        let pop() = 
            callBase BaseMethods.pop

        let push() =
            callBase BaseMethods.push

        let emit = function
            | Load value ->
                match value with
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
                push()
            | Discard ->
                pop()
                il.Emit(OpCodes.Pop)
            | Clear ->
                callBase BaseMethods.clear
            | Flip ->
                pop(); pop()
                emitFlip()
                push(); push()
            | Dup ->
                pop()
                il.Emit(OpCodes.Dup)
                push(); push()
            | InputChar ->
                callBase BaseMethods.inputChar
                push()
            | InputNumber ->
                callBase BaseMethods.inputNumber
                push()
            | OutputChar ->
                pop()
                callBase BaseMethods.outputChar
            | OutputNumber ->
                pop()
                callBase BaseMethods.outputNumber
            | BinOp op ->
                pop(); pop()
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
                    callBase BaseMethods.readText
                push()

            | UnOp op ->
                pop()
                match op with
                | Not ->
                    il.Emit(OpCodes.Ldc_I4_0)
                    il.Emit(OpCodes.Ceq)
                push()

        let emitControl = function
            | Rand targets ->
                let methods =
                    targets
                    |> Seq.map (fun t -> (il.DefineLabel(), fst definedMethods.[t]))
                    |> Seq.toArray
                                             
                callBase BaseMethods.rand

                il.Emit(OpCodes.Switch, methods |> Array.map (fun (lbl, _) -> lbl))

                methods |> Array.iter (fun (lbl, target) -> il.MarkLabel lbl; tailTo target)

            | Branch (zero, nonZero) ->
                let (zMethod, _) = definedMethods.[zero]
                let (nzMethod, _) = definedMethods.[nonZero]
                let zeroTarget = il.DefineLabel()
                pop()
                il.Emit(OpCodes.Brfalse, zeroTarget)
                tailTo nzMethod
                il.MarkLabel(zeroTarget)
                tailTo zMethod

            | Exit -> il.Emit(OpCodes.Ret)

            | ToState n ->
                let (nMethod, _) = definedMethods.[n]
                tailTo nMethod

        List.iter emit tc.chain.instructions
        emitControl tc.chain.control

    Map.iter (fun _ m -> buildMethod m) definedMethods

    defineRunMethod tb initialFunction definedMethods
    defineConstructor tb progText

    let result = tb.CreateType()

    result

