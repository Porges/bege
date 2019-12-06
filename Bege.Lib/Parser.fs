module Bege.Parser

open System.IO

let private lines s =
    seq {
        use str = new StringReader(s)
        let mutable line : string = null
        while (line <- str.ReadLine(); line <> null) do
            line
    }
    
// TODO: Enforce 80x25 restriction on Befunge93?
type Program = BefungeSpace

let parse input : Program =

    let result = BefungeSpace ()

    Seq.zip (lines input) (Seq.initInfinite (fun x -> x))
    |> Seq.iter 
        (fun (line, x) -> 
            for y in 0..line.Length-1 do
                result.[x,y] <- int line.[y])
    
    result
 