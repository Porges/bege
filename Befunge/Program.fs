module Program

open System
open System.IO

open CommandLine

type Options = {
    [<Value(0, Required = true, MetaName = "input", HelpText = "The input file (e.g. \"file.bf\").")>] input : string;
    [<Option('o', "output")>] output : string option;
    [<Option('O', "optimize", Default = true)>] optimize : bool;
}

module ExitCodes =
    let Success = 0
    
    let InvalidOptions = 400
    let InputNotFound = 404

type FatalException(message : string, exitCode : int, innerException : Exception) =
    inherit Exception(message, innerException)
    member e.ExitCode = exitCode

let executeProgram befungeType = 

    let seed = Guid.NewGuid().GetHashCode()
    let c =
        Activator.CreateInstance(befungeType, Console.In, Console.Out, uint64(seed))
        :?> V4.BefungeBase

    c.Run ()

let run options : unit =
    let prog =
        try
            V4.parse (File.ReadAllText options.input)
        with
        | :? FileNotFoundException as ex ->
            let msg = sprintf "File '%s' does not exist." options.input
            raise <| FatalException(msg, ExitCodes.InputNotFound, ex)

    let compiled = V4.compile options.output prog options.optimize

    // if we didn't save an output file then execute it straight away:
    if options.output.IsNone
    then executeProgram compiled |> ignore; printfn ""

[<EntryPoint>]
let main argv = 
    match Parser.Default.ParseArguments<Options> argv with
    | :? Parsed<Options> as p ->
        try
            run p.Value; 0
        with
        | :? FatalException as ex ->
            eprintfn "Fatal: %s" ex.Message
            ex.ExitCode
    | _ -> ExitCodes.InvalidOptions
