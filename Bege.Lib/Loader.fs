module Bege.Loader

open System.IO

let private lines s =
    seq {
        use str = new StringReader(s)
        let mutable line : string = null
        while (line <- str.ReadLine(); line <> null) do
            line
    }
    
// TODO [93]: Enforce 80x25 restriction on Befunge93?
// TODO [Trefunge]: Form-feed should move to next plane

let loadInto (memory: BefungeSpace) (input: string): unit =

    lines input
    |> Seq.iteri
        (fun x line -> 
            for y in 0..line.Length-1 do
                let char = line.[y]
                if char <> ' ' // spaces are 'transparent' per Befunge spec, and do not overwrite anything in memory
                then memory.[x,y] <- int char)
 
let load (input: string): BefungeSpace =

    let result = BefungeSpace ()
    loadInto result input
    result