module Bege.Runtime

open System
open System.IO
open System.Collections.Generic
open System.Reflection
open System.Text

[<AbstractClass>]
type Funge(tr : TextReader, tw : TextWriter, progText : string, seed : uint64) =

    let memory = Parser.parse progText
    let stack = Stack<int>()

    let mutable lcg = LCG.createKnuth seed
    let mutable count = 0

    let addCount(): unit = count <- count + 1

    new(progText : string) = Funge(Console.In, Console.Out, progText, uint64(Guid.NewGuid().GetHashCode()))

    abstract member Run : unit -> int

    member _.GetCount() = count 

    member _.Rand() : int =
        addCount()
        lcg <- LCG.next lcg
        int (LCG.bits 2 lcg)

    member _.Pop() =
        addCount()
        if stack.Count > 0 then stack.Pop() else 0

    member _.Clear() =
        addCount()
        stack.Clear()

    member _.Push (value : int32) : unit =
        addCount()
        stack.Push value

    member _.InputChar() : int32 =
        addCount()
        tr.Read()

    member _.OutputChar(c : int32) : unit =
        addCount()
        tw.Write(char(c))

    member _.InputNumber() : int32 =
        addCount()

        let mutable r = 0

        // first, skip all non-numeric characters
        while (r <- tr.Read(); r <> -1 && not (r >= int('0') && r <= int('9'))) do
            () // skipping

        if r = -1 then
            -1 // EOF
        else
        
            let read = StringBuilder()
            read.Append(char(r)) |> ignore

            let mutable next = 0
            while (next <- tr.Peek(); next >= int('0') && next <= int('9')) do
                read.Append(char(tr.Read())) |> ignore

            match Int32.TryParse (read.ToString()) with
            | (true, value) -> value
            | _ -> raise <| InvalidDataException()

    member _.ReadText(b : int32, a : int32) : int32 =
        addCount()
        memory.[a, b]

    member _.OutputNumber(v : int32) : unit =
        addCount()
        tw.Write(v)
        tw.Write(' ')

        (*
    member x.Interpret(dir, row, column) : unit =
        let rec go dir row col =
            let advance ip row col =
                go ip row col

            match char(fungeSpace.[ip.row, ip.column]) with
            | 'r' -> advance (reflect dir) row col
            | 'v' -> advance Dir.up row col
            | 'p' -> 

        go dir row col
        *)


module BaseMethods = 
    let private m n = typeof<Funge>.GetMethod(n, BindingFlags.Instance ||| BindingFlags.Public)
    let push = m "Push"
    let pop = m "Pop"
    let clear = m "Clear"
    let outputChar = m "OutputChar"
    let inputChar = m "InputChar"
    let inputNumber = m "InputNumber"
    let outputNumber = m "OutputNumber"
    let readText = m "ReadText"
    let rand = m "Rand"
    let ctor = typeof<Funge>.GetConstructor([| typeof<TextReader>; typeof<TextWriter>; typeof<string>; typeof<uint64> |])
    let easyCtor = typeof<Funge>.GetConstructor([| typeof<string> |])
    let count = m "GetCount"