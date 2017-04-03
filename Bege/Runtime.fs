module Bege.Runtime

open System
open System.IO
open System.Collections.Generic
open System.Reflection

[<AbstractClass>]
type BefungeBase(tr : TextReader, tw : TextWriter, progText : string, seed : uint64) =
    let stack = Stack<int>()
    let mutable lcg = LCG.createKnuth seed
    let mutable count = 0

    let addCount() : unit =
        count <- count + 1

    let pop() : int = if stack.Count > 0 then stack.Pop() else 0
    let push(value) : unit = stack.Push value

    abstract member Run : unit -> int

    member x.GetCount() =
        count

    member x.Rand() : int =
        addCount()
        lcg <- LCG.next lcg
        int (LCG.bits 2 lcg)

    member x.Push value : unit =
        addCount()
        push(value)

    member x.Pop() =
        addCount()
        pop()

    member x.Dup() : unit =
        addCount ()
        if stack.Count > 0 then push(stack.Peek()) else push(0)

    member x.Flip() : unit =
        addCount()
        let a = pop()
        let b = pop()
        push(a)
        push(b)

    member x.InputChar() : unit =
        addCount()
        push (tr.Read())

    member x.OutputChar() : unit =
        addCount()
        fprintf tw "%c" (char(pop()))

    member x.InputNumber() : unit =
        addCount()

        let mutable read = ""
        let mutable r = 0
        while (r <- tr.Read(); r <> -1 && r <> int(' ')) do
            read <- read + string(char(r))

        match Int32.TryParse read with
        | (true, value) -> push(value)
        | _ -> raise <| InvalidDataException()

    member x.ReadText() : unit =
        addCount()
        let a = pop()
        let b = pop()
        push(int(progText.[a*25 + b]))

    member x.OutputNumber() : unit =
        addCount()
        fprintf tw "%d " (pop())

    member x.Greater() : unit =
        addCount()
        let a = pop()
        let b = pop()
        push(Convert.ToInt32(b > a))
        
    member x.Not() : unit =
        addCount()
        if pop() = 0 then push(1) else push(0)

    member x.Add() : unit =
        addCount()
        push(pop() + pop())

    member x.Subtract() : unit = 
        addCount()
        let a = pop()
        let b = pop()
        push(b - a)

    member x.Multiply() : unit =
        addCount()
        push(pop() * pop())

    member x.Divide() : unit =
        addCount()
        let a = pop()
        let b = pop()
        push(b / a)

module BaseMethods = 
    let private m n = typeof<BefungeBase>.GetMethod(n, BindingFlags.Instance ||| BindingFlags.Public)
    let pop = m "Pop"
    let push = m "Push"
    let dup = m "Dup"
    let flip = m "Flip"
    let outputChar = m "OutputChar"
    let inputChar = m "InputChar"
    let inputNumber = m "InputNumber"
    let outputNumber = m "OutputNumber"
    let not = m "Not"
    let add = m "Add"
    let subtract = m "Subtract"
    let multiply = m "Multiply"
    let divide = m "Divide"
    let greater = m "Greater"
    let readText = m "ReadText"
    let rand = m "Rand"
    let ctor = typeof<BefungeBase>.GetConstructors() |> Seq.exactlyOne
    let count = m "GetCount"