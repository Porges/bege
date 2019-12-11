module Bege.Loader

open System.IO

let private lines s =
    seq {
        use str = new StringReader(s)
        let mutable line : string = null
        while (line <- str.ReadLine(); line <> null) do
            line
    }
    
// TODO: Enforce 80x25 restriction on Befunge93?

let load input : BefungeSpace =

    let result = BefungeSpace ()
    
    lines input
    |> Seq.iteri
        (fun x line -> 
            for y in 0..line.Length-1 do
                result.[x,y] <- int line.[y])
 
    result
 