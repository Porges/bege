module Bege.Renderer

open System
open System.IO
open System.Reflection
open System.Reflection.Emit

open Bege.AST
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

let saveRunnableAssembly (dynAsm : AssemblyBuilder) dynMod resultType path =
    dynAsm.SetEntryPoint (defineMain dynMod resultType)
    let fileName = Path.GetFileName path
    let dir = Path.GetDirectoryName path
    if not (String.IsNullOrEmpty dir) then Directory.CreateDirectory(dir) |> ignore
    // AssemblyBuilder won't save directly to a path..
    dynAsm.Save(fileName)
    if fileName <> path
    then File.Delete(path)
    File.Move(fileName, path)

let buildType (options : Options) (progText : string) (chains : Map<string, TypedChain<string>>, initialFunction : string) : Type =

    let dynAsm = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)

    let moduleFile = Path.GetFileName <| Option.defaultValue "temp.dll" options.outputFileName

    let dynMod = dynAsm.DefineDynamicModule(moduleName, moduleFile)
    let typeAttributes =
        TypeAttributes.Public |||
        TypeAttributes.Class |||
        TypeAttributes.Sealed |||
        TypeAttributes.AutoLayout

    let tb = dynMod.DefineType(programClassName, typeAttributes, typeof<Runtime.BefungeBase>)

    let definedMethods =
        chains |> Map.map (fun name chain ->
            let args = fst chain.stackBehaviour
            if options.verbose then printfn "Defining %s (%d args)" name args
            let args = Array.replicate 0 (* TODO: args *) typeof<int>
            let method = tb.DefineMethod(name, MethodAttributes.Private, typeof<Void>, args)
            (method, chain))

    let buildMethod fs (method : MethodBuilder, c) =
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
            | Push -> callBase BaseMethods.push
            | Pop -> callBase BaseMethods.pop
            | Discard -> il.Emit(OpCodes.Pop)
            | Clear -> callBase BaseMethods.clear
            | Flip -> emitFlip()
            | Dup -> il.Emit(OpCodes.Dup)
            | InputChar -> callBase BaseMethods.inputChar
            | InputNumber -> callBase BaseMethods.inputNumber
            | OutputChar -> callBase BaseMethods.outputChar
            | OutputNumber -> callBase BaseMethods.outputNumber
            | BinOp Greater ->
                // we use "less than" rather than flipping and
                // calling greater than
                il.Emit(OpCodes.Clt)
            | BinOp Add -> il.Emit(OpCodes.Add)
            | BinOp Multiply -> il.Emit(OpCodes.Mul)
            | UnOp Not ->
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ceq)
            | BinOp Divide ->
                emitFlip()
                il.Emit(OpCodes.Div)
            | BinOp Modulo ->
                emitFlip()
                il.Emit(OpCodes.Rem)
            | BinOp Subtract ->
                emitFlip()
                il.Emit(OpCodes.Sub)
            | BinOp ReadText ->
                emitFlip()
                callBase BaseMethods.readText

        let emitLast = function
            | Rand [a; b; c; d] ->
                let (a, _) = definedMethods.[a]
                let (b, _) = definedMethods.[b]
                let (c, _) = definedMethods.[c]
                let (d, _) = definedMethods.[d]

                callBase BaseMethods.rand

                let aL = il.DefineLabel()
                let bL = il.DefineLabel()
                let cL = il.DefineLabel()
                let dL = il.DefineLabel()

                il.Emit(OpCodes.Switch, [| aL; bL; cL; dL |])

                il.MarkLabel aL; tailTo a
                il.MarkLabel bL; tailTo b
                il.MarkLabel cL; tailTo c
                il.MarkLabel dL; tailTo d
            | Branch (zero, nonZero) ->
                let (zMethod, _) = definedMethods.[zero]
                let (nzMethod, _) = definedMethods.[nonZero]
                let zeroTarget = il.DefineLabel()
                il.Emit(OpCodes.Brfalse, zeroTarget)
                tailTo nzMethod
                il.MarkLabel(zeroTarget)
                tailTo zMethod
            | Exit -> il.Emit(OpCodes.Ret)
            | ToState n ->
                let (nMethod, _) = definedMethods.[n]
                tailTo nMethod

        List.iter emit c.instructions
        emitLast c.lastInstruction

    Map.iter buildMethod definedMethods

    defineRunMethod tb initialFunction definedMethods
    defineConstructor tb progText

    let result = tb.CreateType()

    // if filename is given, define Main and save the assembly
    options.outputFileName |> Option.iter (saveRunnableAssembly dynAsm dynMod result)

    result

