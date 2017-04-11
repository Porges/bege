module Bege.Common

open System

let inline always x _ = x // 'const' was taken
let inline flip f x y = f y x

// applies f to x until x doesn't change any more
let rec fix f x = 
    let x' = f x
    if x' = x then x'
    else fix f x'

let inline invertMap it = 
    let f m k v =
        match Map.tryFind v m with
        | None -> Map.add v [k] m
        | Some k' -> Map.add v (k::k') m

    Map.fold f Map.empty it


module ExitCodes =
    let Success = 0
    
    let InvalidOptions = 400
    let InputNotFound = 404
    let CommandNotSupported = 405

    let StandardNotSupported = 505

type FatalException(message : string, exitCode : int, innerException : Exception) =
    inherit Exception(message, innerException)

    new(message : string, exitCode : int) = FatalException(message, exitCode, null)

    member e.ExitCode = exitCode