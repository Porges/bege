// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module V4

    open System
    open System.IO

    module LCG =

        type [<Struct>] LCG<'a> = { value : 'a; a : 'a; c : 'a }
        let inline next lcg = { lcg with value = lcg.value * lcg.a + lcg.c }

        let createKnuth seed = { value = seed; a = 6364136223846793005UL; c = 1442695040888963407UL }

    open LCG 
    open System.Reflection.Emit
    open System.Reflection
    open System.Diagnostics
    open System.Text

    let lines s = seq {
        use str = new StringReader(s)
        let mutable line : String = null
        while (line <- str.ReadLine(); line <> null) do
            yield line
        }

    let randSort (a, b, c, d) =
        let [x; y; z; w] = List.sort [a; b; c; d]
        (x, y, z, w)

    type Program = char[,]

    let emptyLine = String(' ', 80).ToCharArray()
    let parse p : Program =
        Seq.append (lines p |> Seq.map (fun l -> l.PadRight(80).ToCharArray())) (Seq.initInfinite (fun _ -> emptyLine))
        |> Seq.take 25 |> array2D

    [<AbstractClass>]
    type BefungeBase(tr : TextReader, tw : TextWriter, progText : string, seed : uint64) =
        let stack = System.Collections.Generic.Stack<int>()
        let mutable lcg = createKnuth seed
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
            lcg <- next lcg
            int (lcg.value >>> 62)

        member x.Push value : unit =
            addCount()
            push(value)

        member x.Pop() =
            addCount()
            pop()

        member x.Dup() : unit =
            addCount ();
            if stack.Count > 0 then push(stack.Peek()) else push(0)

        member x.Flip() : unit =
            addCount()
            let a = pop()
            let b = pop()
            push(a)
            push(b)

        member x.InputChar() : unit =
            addCount();
            push (tr.Read())

        member x.OutputChar() : unit =
            addCount();
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
            addCount();
            if pop() = 0 then push(1) else push(0)

        member x.Add() : unit =
            addCount();
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


    type Dir = Right = 0 | Up = 1 | Down = 2 | Left = 3

    [<StructuredFormatDisplay("{dir} {position}")>]
    type [<Struct>] FollowState = { position : struct (int * int) ; dir : Dir }

    let advance (fs : FollowState) = 
        let struct (x, y) = fs.position
        let (x', y') =
            match fs.dir with
            | Dir.Right -> (x, y+1)
            | Dir.Left -> (x, y-1)
            | Dir.Up -> (x-1, y)
            | Dir.Down -> (x+1, y)
        let x' = if x' < 0 then x' + 25 else x' % 25
        let y' = if y' < 0 then y' + 80 else y' % 80
        { fs with position = struct (x', y') }
    
    type Instruction = 
        | Push of int
        | Pop
        | Dup | Flip
        | OutputNumber
        | InputNumber
        | OutputChar
        | InputChar
        | Add | Multiply | Divide | Subtract
        | Not
        | Greater
        | ReadText
    
    type LastInstruction =
        | Exit
        | ToState of next : FollowState
        | Branch of zero : FollowState * nonZero : FollowState
        | Rand of FollowState * FollowState * FollowState * FollowState

    let programEntryState = { position = struct (0,0); dir = Dir.Right }
    let computeChains (prog : Program) : Map<FollowState, Instruction list * LastInstruction> =
        
        let toCompile = System.Collections.Generic.Queue<FollowState>()
        let visited = System.Collections.Generic.HashSet<FollowState>()
        let mutable chains = Map.empty

        let toVisit newState =
            if visited.Add newState
            then toCompile.Enqueue newState

        toVisit programEntryState

        let read (struct (x, y)) = prog.[x, y]

        while toCompile.Count > 0 do

            let rec follow chain (state : FollowState) : (Instruction list * LastInstruction) = 

                let go x = follow (x :: chain) (advance state)

                let branchChain(dir) =
                    let next = advance { state with dir = dir }
                    toVisit next
                    next

                let endChain x = (List.rev chain, x)

                let newChain dir = 
                    let next = advance { state with dir = dir }
                    toVisit next
                    endChain (ToState next)

                match read state.position with
                | '>' -> newChain Dir.Right
                | '<' -> newChain Dir.Left
                | '^' -> newChain Dir.Up
                | 'v' -> newChain Dir.Down
                | d when d >= '0' && d <= '9' -> go (Push (int(d) - int('0'))) 
                | h when h >= 'a' && h <= 'f' -> go (Push (int(h) - int('a') + 10)) // '98 extension
                | '$' -> go Pop
                | ' ' -> follow chain (advance state)
                | '#' -> follow chain (advance (advance state))
                | '"' -> readString chain (advance state)
                | '@' -> endChain Exit
                | '~' -> go InputChar
                | '&' -> go InputNumber
                | ':' -> go Dup
                | '\\' -> go Flip
                | '.' -> go OutputNumber
                | ',' -> go OutputChar
                | '!' -> go Not
                | '?' -> endChain (Rand (randSort (branchChain Dir.Left, branchChain Dir.Right, branchChain Dir.Up, branchChain Dir.Down)))
                | '+' -> go Add
                | '-' -> go Subtract
                | '/' -> go Divide
                | '*' -> go Multiply
                | '_' -> endChain (Branch (branchChain Dir.Right, branchChain Dir.Left))
                | '|' -> endChain (Branch (branchChain Dir.Up, branchChain Dir.Down))
                | '`' -> go Greater
                | 'g' -> go ReadText
                | c -> raise <| NotSupportedException("Instruction not supported: " + string(c))
            and readString acc (state : FollowState) : (Instruction list * LastInstruction) =
                match read state.position with
                | '"' -> follow acc (advance state)
                | c -> readString (Push (int c) :: acc) (advance state)
            
            let start = toCompile.Dequeue()
            chains <- Map.add start (follow [] start) chains

        chains

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
        let ctor = typeof<BefungeBase>.GetConstructor([| typeof<TextReader>; typeof<TextWriter>; typeof<string>; typeof<uint64> |])
        let count = m "GetCount"

    let buildType (fileName : string option) (progText : string) (chains : Map<FollowState, Instruction list * LastInstruction>) : Type =

        let assemblyName = AssemblyName("BefungeAssembly")
        let dynAsm = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
        let dynMod = dynAsm.DefineDynamicModule("BefungeModule", Option.defaultValue "temp.dll" fileName)
        let typeAttributes = 
            TypeAttributes.Public |||
            TypeAttributes.Class |||
            TypeAttributes.Sealed |||
            TypeAttributes.AutoLayout

        let tb = dynMod.DefineType("BefungeProgram", typeAttributes, typeof<BefungeBase>)

        let nameFromState { dir = d; position = struct (x, y) } = sprintf "%A_%d_%d" d x y

        let definedMethods =
            let defineMethod fs chain = 
                // printfn "Defining %s" (nameFromState fs)
                let method = tb.DefineMethod(nameFromState fs, MethodAttributes.Private) in
                (method, chain) in
            chains |> Map.map defineMethod

        let buildMethod fs (method : MethodBuilder, (instructions, lastInstruction)) =
            // printfn "Emitting method: %s" (nameFromState fs)
            let il = method.GetILGenerator()

            let callBase (mi : MethodInfo) =
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Call, mi)
            
            let tailTo (mi : MethodInfo) =
                il.Emit(OpCodes.Ldarg_0)
                il.Emit(OpCodes.Tailcall)
                il.Emit(OpCodes.Call, mi)
                il.Emit(OpCodes.Ret)

            let emit = function
                | Push value ->
                    il.Emit(OpCodes.Ldarg_0)
                    il.Emit(OpCodes.Ldc_I4, value)
                    il.Emit(OpCodes.Call, BaseMethods.push)
                | Pop ->
                    callBase BaseMethods.pop
                    il.Emit(OpCodes.Pop) // ignore result
                | Flip -> callBase BaseMethods.flip
                | Dup -> callBase BaseMethods.dup
                | OutputChar -> callBase BaseMethods.outputChar
                | InputChar -> callBase BaseMethods.inputChar
                | InputNumber -> callBase BaseMethods.inputNumber
                | OutputNumber -> callBase BaseMethods.outputNumber
                | Not -> callBase BaseMethods.not
                | Add -> callBase BaseMethods.add
                | Multiply -> callBase BaseMethods.multiply
                | Divide -> callBase BaseMethods.divide
                | Subtract -> callBase BaseMethods.subtract
                | Greater -> callBase BaseMethods.greater
                | ReadText -> callBase BaseMethods.readText

            let emitLast = function
                | Rand (a, b, c, d) ->
                    let (a, _) = definedMethods.[a]
                    let (b, _) = definedMethods.[b]
                    let (c, _) = definedMethods.[c]
                    let (d, _) = definedMethods.[d]

                    callBase BaseMethods.rand

                    let aL = il.DefineLabel()
                    let bL = il.DefineLabel()
                    let cL = il.DefineLabel()
                    let dL = il.DefineLabel()

                    il.Emit(OpCodes.Switch, [| aL; bL; cL; dL |])

                    il.MarkLabel aL; tailTo a
                    il.MarkLabel bL; tailTo b
                    il.MarkLabel cL; tailTo c
                    il.MarkLabel dL; tailTo d
                | Branch (zero, nonZero) ->
                    let (zMethod, _) = definedMethods.[zero]
                    let (nzMethod, _) = definedMethods.[nonZero]
                    callBase BaseMethods.pop
                    let zeroTarget = il.DefineLabel()
                    il.Emit(OpCodes.Brfalse, zeroTarget)
                    tailTo nzMethod
                    il.MarkLabel(zeroTarget)
                    tailTo zMethod
                | Exit -> il.Emit(OpCodes.Ret)
                | ToState n -> 
                    let (nMethod, _) = definedMethods.[n]
                    tailTo nMethod
            
            List.iter emit instructions
            emitLast lastInstruction

        Map.iter buildMethod definedMethods

        let defineRunMethod() =
            let runMethod = tb.DefineMethod("Run", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<int>, [||])
            let (entryMethod, _) = definedMethods.[programEntryState]
            let runIL = runMethod.GetILGenerator()
            runIL.Emit(OpCodes.Ldarg_0)
            runIL.Emit(OpCodes.Call, entryMethod)
            runIL.Emit(OpCodes.Ldarg_0)
            runIL.Emit(OpCodes.Call, BaseMethods.count)
            runIL.Emit(OpCodes.Ret)

        defineRunMethod()

        let defineConstructor() =
            let ctor = 
                tb.DefineMethod(
                    ".ctor",
                    MethodAttributes.Public ||| MethodAttributes.SpecialName,
                    typeof<Void>,
                    [| typeof<TextReader>; typeof<TextWriter>; typeof<uint64> |])

            let il = ctor.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Ldstr, progText)
            il.Emit(OpCodes.Ldarg_3)
            il.Emit(OpCodes.Call, BaseMethods.ctor)
            il.Emit(OpCodes.Ret)

        defineConstructor()

        let result = tb.CreateType()

        Option.iter dynAsm.Save fileName

        result

    // applies f to x until x doesn't change any more
    let rec fix f x = 
        let x' = f x
        if x' = x then x'
        else fix f x'

    let invertMap : Map<'k, 'v> -> Map<'v, 'k list> = 
        Map.fold (fun m k v -> 
            match Map.tryFind v m with
            | None -> Map.add v [k] m
            | Some k' -> Map.add v (k::k') m) Map.empty
    
    let optimize (chains : Map<FollowState, Instruction list * LastInstruction>) : Map<FollowState, Instruction list * LastInstruction> = 

        let inlineChains (m : Map<FollowState, Instruction list * LastInstruction>) =

            let inc k m =
                match Map.tryFind k m with
                | None -> Map.add k 1 m
                | Some c -> Map.add k (c+1) m

            let countReferences m =
                    m |> Map.fold (fun cs _ (_, li) -> 
                        match li with
                        | Exit -> cs
                        | Branch (l, r) -> inc l (inc r cs)
                        | Rand (a, b, c, d) -> inc a (inc b (inc c (inc d cs)))
                        | ToState n -> inc n cs) Map.empty
             
            let counts = countReferences m

            let canBeInlined fs =
                counts.[fs] = 1
            
            let isJump fs =
                match m.[fs] with
                | ([], ToState t) -> true
                | _ -> false
            
            let target fs = 
                match m.[fs] with
                | ([], ToState t) -> t

            let result =
                m
                |> Map.map (fun fs ((il, li) as value) ->
                    match li with
                    | ToState n when canBeInlined n || isJump n ->
                        // printfn "Inlining %A into %A" n fs
                        let il', li' = m.[n] in
                        (List.append il il', li')
                    | Branch (l, r) when isJump l || isJump r ->
                        let l = if isJump l then target l else l
                        let r = if isJump r then target r else r
                        (il, Branch (l, r))
                    | Rand (a, b, c, d)
                        when isJump a || isJump b || isJump c || isJump d ->
                        let a = if isJump a then target a else a
                        let b = if isJump b then target b else b
                        let c = if isJump c then target c else c
                        let d = if isJump d then target d else d
                        (il, Rand (randSort (a, b, c, d)))
                    | _ ->  value)

            let newCounts = countReferences result

            result |> Map.filter (fun k _ -> newCounts.ContainsKey k || k = programEntryState (* can't remove entry state *))

        let rec optimizeChain = function
            // constant folding
            | Push x :: Push y :: Add :: is -> optimizeChain (Push (x + y) :: is)
            | Push x :: Push y :: Divide :: is -> optimizeChain (Push (x / y) :: is)
            | Push x :: Push y :: Multiply :: is -> optimizeChain (Push (x * y) :: is)
            | Push x :: Push y :: Subtract :: is -> optimizeChain (Push (x - y) :: is)
            | Push x :: Push y :: Greater :: is -> optimizeChain (Push (Convert.ToInt32(x > y)) :: is)
            // eliminate unneeded flips
            | Push x :: Push y :: Flip :: is -> optimizeChain (Push y :: Push x :: is)
            // eliminate dead pushes
            | Push x :: Pop :: is -> optimizeChain is
            // identical branches
            | (i :: is) -> i :: optimizeChain is
            | [] -> []

        let optimizeLast fs = function
            | Branch (l, r) when l = r -> ToState r
            | Rand (a, b, c, d) when a = b && b = c && c = d -> ToState d

            // Two orders to check here - since Rand is always ordered,
            // either fs < all or fs > all:
            | Rand (a, b, c, d) when a = fs && b = c && c = d -> ToState d
            | Rand (a, b, c, d) when a = fs && b = fs && c = d -> ToState d
            | Rand (a, b, c, d) when a = fs && b = fs && c = fs -> ToState d

            | Rand (a, b, c, d) when a = b && b = c && d = fs -> ToState a
            | Rand (a, b, c, d) when a = b && c = fs && d = fs -> ToState a
            | Rand (a, b, c, d) when b = fs && c = fs && d = fs -> ToState a

            | c -> c

        let optimizeChains = Map.map (fun fs (insns, last) -> (fix optimizeChain insns, optimizeLast fs last))

        let collapseIdenticalChains (m : Map<FollowState, Instruction list * LastInstruction>) : Map<FollowState, Instruction list * LastInstruction> =
            let states1 = m |> Map.toSeq |> Seq.map fst

            // invert the map, now all states with the same instruction list are in the same slot
            let inverted = invertMap m
            let states2 = inverted |> Map.toSeq |> Seq.map fst

            // pick one state to represent the others (the 'minimum')
            let newMappings : Map<FollowState, FollowState> =
                Map.fold (fun m _ fss ->
                    let min = List.min fss in
                    List.fold (fun m fs -> Map.add fs min m) m fss) Map.empty inverted

            let states2 = newMappings |> Map.toSeq |> Seq.map fst

            // rewrite all the last-instructions to remap their states
            inverted
            |> Map.toSeq
            |> Seq.map (fun ((is, last), fss) -> 
                let newLast =
                    match last with
                    | Exit -> Exit
                    | Branch (one, two) -> Branch (newMappings.[one], newMappings.[two])
                    | Rand (a, b, c, d) -> Rand (randSort (newMappings.[a], newMappings.[b], newMappings.[c], newMappings.[d]))
                    | ToState n -> ToState (newMappings.[n])
                (List.min fss, (is, newLast)))
            |> Map.ofSeq

        fix (optimizeChains << inlineChains << collapseIdenticalChains) chains
    
    let toText (prog : Program) : string =
        let sb = StringBuilder(Array2D.length1 prog * Array2D.length2 prog)
        prog |> Array2D.iter (fun c -> sb.Append c |> ignore)
        sb.ToString()

    let compile (fileName : string option) (prog : Program) shouldOptimize : Type =
        computeChains prog 
        |> (fun p -> if shouldOptimize then optimize p else p)
        |> buildType fileName (toText prog)

    let run prog (seed : uint64) (input : TextReader) (output : TextWriter) =
        let compiled = compile None prog true
        
        let instance =
            Activator.CreateInstance(compiled, input, output, seed)
            :?> BefungeBase
        
        instance.Run()


    module Samples =

        let p1 = 
            ">              v\n" +
            "v  ,,,,,\"Hello\"<\n" +
            ">48*,          v\n" +
            "v,,,,,,\"World!\"<\n" +
            ">25*,@"

        let p2 = 
            " >25*\"!dlrow ,olleH\":v\n" +
            "                  v:,_@\n" +
            "                  >  ^\n"

        let p3 = 
            " v>>>>>v\n" +
            "  12345\n" +
            "  ^?^\n" +
            " > ? ?^\n" +
            "  v?v\n" +
            "  6789\n" +
            "  >>>> v\n" +
            " ^    .<"

    module Tests = 
        open Xunit
        open System.IO
        open ApprovalTests

        let private verify code input output =
            use inS = new StringReader(input)
            use outS = new StringWriter()
            let count = run (parse code) 0UL inS outS

            Assert.Equal(output, outS.ToString()) 

        let private verifyOptimized code input output expectedInsns =
            use inS = new StringReader(input)
            use outS = new StringWriter()
            let count = run (parse code) 0UL inS outS

            Assert.Equal(output, outS.ToString()) 
            Assert.Equal(expectedInsns, count)

        [<Theory>] 
        [<InlineData("@", "", "", 0)>]
        [<InlineData("99*76*+.@", "", "123 ", 2)>]
        [<InlineData("&,@", "65 ", "A", 2)>]
        [<InlineData("~.@", "A", "65 ", 2)>]
        [<InlineData("665+*1-,@", "", "A", 2)>]
        [<InlineData("665+*1-.@", "", "65 ", 2)>]
        [<InlineData(">123...@", "", "3 2 1 ", 6)>]
        [<InlineData(">123#...@", "", "3 2 ", 5)>]
        [<InlineData("123.$.@", "", "3 1 ", 6)>]
        [<InlineData("123\\...@", "", "2 3 1 ", 6)>]
        [<InlineData("65`.@", "", "1 ", 2)>]
        [<InlineData("25`.@", "", "0 ", 2)>]
        let specExample code input output insns =
            verifyOptimized code input output insns

        [<Theory>]
        [<InlineData("\"ver\",,,@", "", "rev")>]

        [<InlineData("1!.@", "", "0 ")>]
        [<InlineData("2!.@", "", "0 ")>]
        [<InlineData("0!.@", "", "1 ")>]

        [<InlineData("1..@", "", "1 0 ")>] // popping empty stack produces 0

        [<InlineData("1:..@", "", "1 1 ")>] // dup works

        [<InlineData("~:,,@", "A", "AA")>]

        [<InlineData("<@,~", "A", "A")>]

        let myTests code input output =
            verify code input output

        [<Theory>]
        [<InlineData("samples-factorial.bf", "1 ", "1 ")>]
        [<InlineData("samples-factorial.bf", "2 ", "2 ")>]
        [<InlineData("samples-factorial.bf", "3 ", "6 ")>]
        [<InlineData("samples-factorial.bf", "5 ", "120 ")>]
        // [<InlineData("samples-sieve.bf", "10 ", "2 ")>]
        // uses 'p'
        [<InlineData("samples-convert.bf", "", "234 ")>]
        let sampleFiles file input output = 
            verify (File.ReadAllText file) input output

        [<Theory>]
        [<InlineData("64+\"!dlroW ,olleH\">:#,_@", "", "Hello, World!\n")>]
        [<InlineData("~:1+!#@_,", "this is cat", "this is cat")>]
        let samplePrograms code input output = 
            verify code input output
        
        [<Theory>]
        [<InlineData("01->1# +# :# 0# g# ,# :# 5# 8# *# 4# +# -# _@")>]
        // [<InlineData("0v\n<@_ #! #: #,<*2-1*92,*25,+*92*4*55.0")>]
        // [<InlineData(":0g,:\"~\"`#@_1+0\"Quines are Fun\">_")>]
        // ^ inserts extraneous spaces
        let quines q = 
            verify q "" q
