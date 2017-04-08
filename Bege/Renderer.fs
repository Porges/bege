module Bege.Renderer

open System
open System.IO
open System.Reflection
open System.Reflection.Emit

open Bege.AST
open Bege.Runtime

let assemblyName = AssemblyName "BefungeAssembly"
let moduleName = "BefungeModule"
let programClassName = "BefungeProgram"

let defineRunMethod (tb : TypeBuilder) (definedMethods : Map<IPState, MethodBuilder * Chain>) : unit =
    let runMethod = tb.DefineMethod("Run", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<int>, Type.EmptyTypes)
    let (entryMethod, _) = definedMethods.[programEntryState]
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

let buildType (fileName : string option) (progText : string) (chains : AST.Program) : Type =

    let dynAsm = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)

    let dynMod = dynAsm.DefineDynamicModule(moduleName, Option.defaultValue "temp.dll" fileName)
    let typeAttributes = 
        TypeAttributes.Public |||
        TypeAttributes.Class |||
        TypeAttributes.Sealed |||
        TypeAttributes.AutoLayout

    let tb = dynMod.DefineType(programClassName, typeAttributes, typeof<Runtime.BefungeBase>)

    let nameFromState { dir = d; position = struct (x, y) } = sprintf "%A_%d_%d" d x y

    let definedMethods =
        let defineMethod fs chain = 
            // printfn "Defining %s" (nameFromState fs)
            let method = tb.DefineMethod(nameFromState fs, MethodAttributes.Private) in
            (method, chain) in
        chains |> Map.map defineMethod

    let buildMethod fs (method : MethodBuilder, (instructions, lastInstruction)) =
        // printfn "Emitting method: %s" (nameFromState fs)
        let il = method.GetILGenerator()

        let callBase (mi : MethodInfo) =
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Call, mi)
        
        let tailTo (mi : MethodInfo) =
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Tailcall)
            il.Emit(OpCodes.Call, mi)
            il.Emit(OpCodes.Ret)

        let emit = function
            | Push value ->
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Ldc_I4, value)
                il.Emit(OpCodes.Call, BaseMethods.push)
            | Pop ->
                callBase BaseMethods.pop
                il.Emit(OpCodes.Pop) // ignore result
            | Flip -> callBase BaseMethods.flip
            | Dup -> callBase BaseMethods.dup
            | OutputChar -> callBase BaseMethods.outputChar
            | InputChar -> callBase BaseMethods.inputChar
            | InputNumber -> callBase BaseMethods.inputNumber
            | OutputNumber -> callBase BaseMethods.outputNumber
            | Not -> callBase BaseMethods.not
            | Add -> callBase BaseMethods.add
            | Multiply -> callBase BaseMethods.multiply
            | Divide -> callBase BaseMethods.divide
            | Subtract -> callBase BaseMethods.subtract
            | Greater -> callBase BaseMethods.greater
            | ReadText -> callBase BaseMethods.readText

        let emitLast = function
            | Rand (a, b, c, d) ->
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
                callBase BaseMethods.pop
                let zeroTarget = il.DefineLabel()
                il.Emit(OpCodes.Brfalse, zeroTarget)
                tailTo nzMethod
                il.MarkLabel(zeroTarget)
                tailTo zMethod
            | Exit -> il.Emit(OpCodes.Ret)
            | ToState n -> 
                let (nMethod, _) = definedMethods.[n]
                tailTo nMethod
        
        List.iter emit instructions
        emitLast lastInstruction

    Map.iter buildMethod definedMethods

    defineRunMethod tb definedMethods
    defineConstructor tb progText

    let result = tb.CreateType()

    // if filename is given, define Main and save the assembly
    fileName |> Option.iter (fun fn -> dynAsm.SetEntryPoint (defineMain dynMod result); dynAsm.Save fn) 

    result

