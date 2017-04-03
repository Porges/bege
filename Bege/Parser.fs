module Bege.Parser

open System
open System.IO

open Common

let private lines s =
    seq {
        use str = new StringReader(s)
        let mutable line : string = null
        while (line <- str.ReadLine(); line <> null) do
            yield line
    }
    
// A program is an 80x25 array of characters:
type Program = char[,]

let private emptyLine = String(' ', 80).ToCharArray()
let private padding = Seq.initInfinite (always emptyLine)

let parse p : Program =
    lines p
    |> Seq.map (fun l -> l.PadRight(80).ToCharArray())
    |> flip Seq.append padding
    |> Seq.take 25
    |> array2D