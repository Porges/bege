module Bege.Options

open System
open System.Text.RegularExpressions

// CommandLineParser case-insensitivity doesn't work at the moment
// FUTURE: write own command line parser :)
type Topology = Unefunge | Befunge | Trefunge

type Standard = { topology : Topology ; year : int }
    with 
    static member TryParse s =
        let m = Regex.Match(s, @"^(?<topo>une|be|tre)funge(?<year>93|98)\z", RegexOptions.IgnoreCase)
        if not m.Success
        then None
        else
            let topo =
                match m.Groups.["topo"].Value with
                | "une" -> Unefunge
                | "be" -> Befunge
                | "tre" -> Trefunge
                | _ -> failwith "unpossible!"
            
            let year = Int32.Parse(m.Groups.["year"].Value)

            Some { topology = topo; year = year }
    override this.ToString() =
        this.topology.ToString() + this.year.ToString()

let befunge93 = { topology = Befunge; year = 93 }
let befunge98 = { topology = Befunge; year = 98 }

type Options =
    { standard : Standard
    ; optimize : bool
    ; outputFileName : string option
    }