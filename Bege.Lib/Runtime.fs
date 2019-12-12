namespace Bege.Runtime

open Bege
open Bege.Options
open Bege.Parser
open Bege.Optimizer

open System
open System.Collections.Generic
open System.IO

type Funge(options: Options, tr : TextReader, tw : TextWriter, progText : string, seed : uint64) =

    let memory = Loader.load progText
    let stack = Stack<int>()

    let chains = List<Action<Bege.Renderer.FungeFace>>()

    // note that only one of these is used
    let optimizedLookup = Dictionary<Optimizer.StateId, int>()
    let nonOptimizedLookup = Dictionary<InstructionPointer.State, int>()

    let mutable lcg = LCG.createKnuth seed
    let mutable count = 0

    let addCount(): unit = count <- count + 1

    let parse ip = Parser.parseChain options memory ip


    let optimize stack emitAndInvoke chain: Parser.Chain<int> = 
        if options.optimize
        then
            let getId x = 
                match optimizedLookup.TryGetValue(x) with
                | (true, ix) -> ix
                | _ -> 
                    let ix = chains.Count
                    optimizedLookup.[x] <- ix
                    fprintfn options.verbose "Mapped %A -> %d" x ix
                    chains.Add(emitAndInvoke x ix)
                    ix

            let it = (Optimizer.optimize options memory stack chain).chain
            { start = getId it.start
            ; instructions = it.instructions
            ; control = Control.map getId it.control
            }

        else
            let getId x = 
                match nonOptimizedLookup.TryGetValue(x) with
                | true, ix -> ix
                | _ -> 
                    let ix = chains.Count
                    nonOptimizedLookup.[x] <- ix
                    fprintfn options.verbose "Mapped %A -> %d" x ix
                    chains.Add(emitAndInvoke (x, Stack.unknownStack) ix)
                    ix

            { start = getId chain.start
            ; instructions = chain.instructions
            ; control = Control.map getId chain.control
            }
            
    let render chain = Renderer.defineMethod options chain

    let chainFor (start, stack) emitAndInvoke = start |> parse |> optimize stack emitAndInvoke |> render 

    new(options: Options, progText : string) = Funge(options, Console.In, Console.Out, progText, uint64(Guid.NewGuid().GetHashCode()))

    member this.Run(): unit =
        let rec emitAndInvoke (start, stack) ix = Action<Bege.Renderer.FungeFace>(fun this ->
            let method = chainFor (start, stack) emitAndInvoke
            chains.[ix] <- method
            method.Invoke(this))

        (emitAndInvoke (InstructionPointer.programEntryState, Stack.emptyStack) 0).Invoke(this)

    member _.GetCount() = count 

    interface Bege.Renderer.FungeFace with 
        member this.Call(which: int): unit =
            chains.[which].Invoke(this)

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
            
                let read = System.Text.StringBuilder()
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
