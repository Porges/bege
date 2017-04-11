module Bege.Runtime

open System
open System.IO
open System.Collections.Generic
open System.Reflection

(*
 Befunge 98 space is (int32, int32)-indexed space.

 We instantiate a block at a time to try to increase locality and reduce the
 number of entries, rather than one dictionary entry per memory cell.

 The blocks are 0x3F * 0x3F in size, giving just under 4000 cells per block.
 This was chosen to be on the order of the Befunge-93 space of 80x25 = 2000 cells.
*)
type Funge98Space() =
    let storage = System.Collections.Generic.Dictionary<int64, char[]>()

    let bitMask = 0x3F
    let blockSize = (bitMask + 1) * (bitMask + 1)
    let notMask = ~~~0x3F
    let ix x y = (int64(x &&& notMask) <<< 32) ||| int64(y &&& notMask)
    let subIx x y = ((x &&& bitMask) <<< 6) ||| (y &&& bitMask)

    // Cache the last-used block to avoid dict lookups:
    let mutable lastIx : int64 = 0L
    let mutable lastArr : char[] = null

    let newBlock ix =
        let result = Array.create blockSize ' '
        storage.Add(ix, result)
        result

    do 
        lastArr <- newBlock lastIx

    member this.Item
        with get(x : int32, y : int32) =
            let index = ix x y
            let subIndex = subIx x y
            if lastIx = index
            then
                lastArr.[subIndex]
            else
                match storage.TryGetValue index with
                | (true, arr) ->
                    lastIx <- index
                    lastArr <- arr
                    arr.[subIndex]
                | (false, _) -> ' '

        and set(x : int32, y : int32) (c : char) =
            let index = ix x y
            let subIndex = subIx x y
            if lastIx = index
            then
                lastArr.[subIndex] <- c
            else
                let arr =
                    match storage.TryGetValue index with
                    | (true, arr) -> arr
                    | (false, _) -> newBlock index

                lastIx <- index
                lastArr <- arr

                arr.[subIndex] <- c

[<AbstractClass>]
type BefungeBase(tr : TextReader, tw : TextWriter, progText : string, seed : uint64) =

    let stack = Stack<int>()
    let mutable lcg = LCG.createKnuth seed
    let mutable count = 0

    new(progText : string) = BefungeBase(Console.In, Console.Out, progText, uint64(Guid.NewGuid().GetHashCode()))

    abstract member Run : unit -> int

    member x.Text
        with get() = progText

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
        int32(x.Text.[a*25 + b])

    static member OutputNumber(v : int32, x : BefungeBase) : unit =
        x.AddCount()
        fprintf x.Writer "%d " v

    static member Greater(a : int32, b : int32) : int32 =
        //x.AddCount()
        Convert.ToInt32(b > a)
        
    static member Not(v : int32) : int32 =
        //addCount()
        if v = 0 then 1 else 0

    member x.Interpret() : unit =
        ()

module BaseMethods = 
    let private m n = typeof<BefungeBase>.GetMethod(n, BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public)
    let pop = m "Pop"
    let push = m "Push"
    let outputChar = m "OutputChar"
    let inputChar = m "InputChar"
    let inputNumber = m "InputNumber"
    let outputNumber = m "OutputNumber"
    let not = m "Not"
    let greater = m "Greater"
    let readText = m "ReadText"
    let rand = m "Rand"
    let ctor = typeof<BefungeBase>.GetConstructor([| typeof<TextReader>; typeof<TextWriter>; typeof<string>; typeof<uint64> |])
    let easyCtor = typeof<BefungeBase>.GetConstructor([| typeof<string> |])
    let count = m "GetCount"