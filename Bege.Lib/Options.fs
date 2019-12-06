namespace Bege.Options

open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions

type Topology = Unefunge = 1| Befunge = 2 | Trefunge = 3

[<StructuredFormatDisplay("{topology}{year}")>]
type Standard =
    { [<CompiledName("Topology")>] topology : Topology
    ; [<CompiledName("Year")>]     year : int }
    with 
    static member TryParse value =
        let m = Regex.Match(value, @"^(?<topo>une|be|tre)funge(?<year>93|98)\z", RegexOptions.IgnoreCase)
        if not m.Success
        then None
        else
            Some <| (
                let topo =
                    match m.Groups.["topo"].Value with
                    | "une" -> Topology.Unefunge
                    | "be" -> Topology.Befunge
                    | "tre" -> Topology.Trefunge
                    | _ -> failwith "unpossible!"
            
                let year = Int32.Parse(m.Groups.["year"].Value)

                { topology = topo; year = year })

    [<CompiledName("TryParse")>]
    static member TryParseOut (value, [<Out>] result : Standard byref) =
        match Standard.TryParse value with
        | None ->
            false
        | Some (standard) ->
            result <- standard
            true

    static member Befunge93 = { topology = Topology.Befunge; year = 93 }
    static member Befunge98 = { topology = Topology.Befunge; year = 98 }

type Options =
    { standard : Standard
    ; optimize : bool
    ; verbose : bool
    }
        