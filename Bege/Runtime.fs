module Bege.Runtime

open System
open System.IO
open System.Collections.Generic
open System.Reflection

open Bege.InstructionPointer
open Bege.Parser

[<AbstractClass>]
type BefungeBase(tr : TextReader, tw : TextWriter, progText : string, seed : uint64) =

    let fungeSpace = Parser.parse progText
    let stack = Stack<int>()
    let mutable lcg = LCG.createKnuth seed
    let mutable count = 0

    new(progText : string) = BefungeBase(Console.In, Console.Out, progText, uint64(Guid.NewGuid().GetHashCode()))

    abstract member Run : unit -> int

    member x.Memory
        with get() = fungeSpace

    member x.Reader
        with get() = tr

    member x.Writer
        with get() = tw
    
    member x.Stack
        with get() = stack

    member x.AddCount() : unit =
        count <- count + 1

    member x.GetCount() =
        count

    member x.Rand() : int =
        x.AddCount()
        lcg <- LCG.next lcg
        int (LCG.bits 2 lcg)

    member x.Pop() =
        x.AddCount()
        if stack.Count > 0 then stack.Pop() else 0

    member x.Clear() =
        x.AddCount()
        stack.Clear()

    static member Push (value : int32, x : BefungeBase) : unit =
        x.AddCount()
        x.Stack.Push value

    member x.InputChar() : int32 =
        x.AddCount()
        tr.Read()

    static member OutputChar(c : int32, x : BefungeBase) : unit =
        x.AddCount()
        fprintf x.Writer "%c" (char(c))

    member x.InputNumber() : int32 =
        x.AddCount()

        let mutable read = ""
        let mutable r = 0
        while (r <- tr.Read(); r <> -1 && r <> int(' ')) do
            read <- read + string(char(r))

        match Int32.TryParse read with
        | (true, value) -> value
        | _ -> raise <| InvalidDataException()

    static member ReadText(b : int32, a : int32, x : BefungeBase) : int32 =
        x.AddCount()
        x.Memory.[a, b]

    static member OutputNumber(v : int32, x : BefungeBase) : unit =
        x.AddCount()
        fprintf x.Writer "%d " v

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
    let private m n = typeof<BefungeBase>.GetMethod(n, BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public)
    let push = m "Push"
    let pop = m "Pop"
    let clear = m "Clear"
    let outputChar = m "OutputChar"
    let inputChar = m "InputChar"
    let inputNumber = m "InputNumber"
    let outputNumber = m "OutputNumber"
    let readText = m "ReadText"
    let rand = m "Rand"
    let ctor = typeof<BefungeBase>.GetConstructor([| typeof<TextReader>; typeof<TextWriter>; typeof<string>; typeof<uint64> |])
    let easyCtor = typeof<BefungeBase>.GetConstructor([| typeof<string> |])
    let count = m "GetCount"