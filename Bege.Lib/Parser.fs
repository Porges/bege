module Bege.Parser

open System
open System.IO

open Bege.FungeSpace

let private lines s =
    seq {
        use str = new StringReader(s)
        let mutable line : string = null
        while (line <- str.ReadLine(); line <> null) do
            yield line
    }
    
// TODO: Enforce 80x25 restriction on Befunge93?
type Program = Funge98Space

let parse p : Program =
    Seq.zip (lines p) (Seq.initInfinite (fun x -> x))
    |> Seq.fold
        (fun program (line, x) -> 
            for y in 0..line.Length-1 do
                program.[x,y] <- int line.[y]
            program)
        (Program())
    
