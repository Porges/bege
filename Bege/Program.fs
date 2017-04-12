module Program

open System
open System.IO

open CommandLine

open Bege.Common
open Bege.Options

type Arguments =
    { [<Value(0, Required = true, MetaName = "input", HelpText = "The input file (e.g. \"file.bf\").")>] input : string
    ; [<Option('o', "output")>] output : string option
    ; [<Option('O', "optimize", Default = true)>] optimize : bool
    ; [<Option("std", Default = "befunge98")>] standard : string
    }

let executeProgram befungeType = 

    let seed = Guid.NewGuid().GetHashCode()
    let c =
        Activator.CreateInstance(befungeType, Console.In, Console.Out, uint64(seed))
        :?> Bege.Runtime.BefungeBase

    c.Run ()

let run input (options : Options) : unit =
    let code =
        try
            File.ReadAllText input
        with
        | :? FileNotFoundException as ex ->
            let msg = sprintf "File '%s' does not exist." input
            raise <| FatalException(msg, ExitCodes.InputNotFound, ex)

    let compiled = Bege.Compiler.compile options code

    // if we didn't save an output file then execute it straight away:
    if options.outputFileName.IsNone
    then executeProgram compiled |> ignore; printfn ""

[<EntryPoint>]
let main argv = 
    match Parser.Default.ParseArguments<Arguments> argv with
    | :? Parsed<Arguments> as p ->
        try
            let args = p.Value
            match Standard.TryParse args.standard with
            | Some standard ->
                run args.input 
                    { standard = standard
                    ; outputFileName = args.output
                    ; optimize = args.optimize
                    }
                0
            | None ->
                raise <| FatalException(sprintf "Unsupported standard: %s" args.standard, ExitCodes.StandardNotSupported)
        with
        | :? FatalException as ex ->
            eprintfn "[Fatal] %s" ex.Message
            ex.ExitCode
    | _ -> ExitCodes.InvalidOptions
