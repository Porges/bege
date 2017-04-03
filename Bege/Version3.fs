// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

namespace V3

module V3 =

    open System
    open System.IO

    module LCG =

        type [<Struct>] LCG<'a> = { value : 'a; a : 'a; c : 'a }
        let inline next lcg = { lcg with value = lcg.value * lcg.a + lcg.c }

        let createKnuth seed = { value = seed; a = 6364136223846793005UL; c = 1442695040888963407UL }

    open LCG 

    type [<Struct>] State = { cursor : Cursor; dir : struct (int * int); stack : int list; stringMode : bool; rand : LCG<uint64> }
    and Program = char[,]
    and [<Struct>] Cursor = { program : Program; row : int; column : int }
        with 
            member c.Read() = c.program.[c.row, c.column]
            member c.Move(struct (x, y)) =
                { c with row = (c.row + x) % 25; column = (c.column + y) % 80 }

    let popped x =
        match x with
        | ({ stack = x :: rest } as s) -> struct (x, { s with stack = rest })
        | s -> struct (0, s) // empty stack pops 0

    let applyOp op s =
        let struct (a, s) = popped s
        let struct (b, s) = popped s
        { s with stack = op b a :: s.stack }

    let applyOp1 op s = 
        let struct (a, s) = popped s
        { s with stack = op a :: s.stack }

    let setDir newDir s = { s with dir = newDir }

    let up = struct (-1 ,0)
    let down = struct (1, 0)
    let right = struct (0, 1)
    let left = struct (0, -1)

    let move (s : State) = { s with cursor = s.cursor.Move(s.dir) }

    let step (s : State) (input : TextReader) (output : TextWriter) : State =
        let c = s.cursor.Read()
        if s.stringMode
        then
            match c with
            | '"' -> { s with stringMode = false }
            | _ -> { s with stack = int c :: s.stack }
        else
            match c with
            | ' ' -> s
            | '>' -> setDir right s
            | '<' -> setDir left s
            | 'v' -> setDir down s
            | '^' -> setDir up s
            | d when d >= '0' && d <= '9'
                -> { s with stack = (int(d) - int('0')) :: s.stack }
            | '+' -> applyOp (+) s
            | '-' -> applyOp (-) s
            | '*' -> applyOp (*) s
            | '/' -> applyOp (/) s
            | '%' -> applyOp (%) s
            | '!' -> applyOp1 (fun x -> if x = 0 then 1 else 0) s
            | '`' -> applyOp (fun b a -> if b > a then 1 else 0) s
            | '_' ->
                let struct (x, s) = popped s
                setDir (if x = 0 then right else left) s
            | '|' ->
                let struct (x, s) = popped s
                setDir (if x = 0 then up else down) s
            | '"' -> { s with stringMode = true }
            | ':' -> 
                let struct (x, s) = popped s
                { s with stack = x :: x :: s.stack }
            | '\\' ->
                let struct (x, s) = popped s
                let struct (y, s) = popped s
                { s with stack = y :: x :: s.stack }
            | '$' -> let struct (_, s) = (popped s) in s
            | '.' -> 
                let struct (x, s) = popped s
                fprintf output "%d " x; s
            | ',' ->
                let struct (x, s) = popped s
                fprintf output "%c" (char x); s
            | '#' -> move s
            | '@' -> raise <| Exception("Done")
            | '?' ->
                let nextRand = next s.rand
                let dir =
                    match nextRand.value >>> 62 with
                    | 0UL -> left
                    | 1UL -> right
                    | 2UL -> up
                    | 3UL -> down

                { s with dir = dir; rand = nextRand }
            | '&' -> 
                let mutable read = ""
                let mutable r = 0
                while (r <- input.Read(); r <> -1 && r <> int(' ')) do
                    read <- read + string(char(r))

                match Int32.TryParse read with
                | (true, value) -> { s with stack = value :: s.stack }
                | _ -> raise <| InvalidDataException()
                
            | '~' -> { s with stack = input.Read() :: s.stack }
            | 'g' ->
                let struct (x, s) = popped s
                let struct (y, s) = popped s
                
                { s with stack = int(s.cursor.program.[x, y]) :: s.stack }
            | _ -> s

    let lines s = seq {
        use str = new StringReader(s)
        let mutable line : String = null
        while (line <- str.ReadLine(); line <> null) do
            yield line
        }

    let emptyLine = String(' ', 80).ToCharArray()
    let parse p : Program =
        Seq.append (lines p |> Seq.map (fun l -> l.PadRight(80).ToCharArray())) (Seq.initInfinite (fun _ -> emptyLine))
        |> Seq.take 25 |> array2D

    let initialState p seed = 
        { cursor = { program = p; row = 0; column = 0 } 
        ; dir = right
        ; stack = []
        ; stringMode = false
        ; rand = createKnuth seed
        }

    let run (prog : Program) seed input output : unit =

        let rec run' (s : State) =
            run' (move (step s input output))

        try
            run' (initialState prog seed)
        with
        | ex -> printfn "%s" ex.Message
            
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
        V3.run (V3.parse code) 0UL inS outS

        Assert.Equal(output, outS.ToString()) 

    [<InlineData("@", "", "")>]
    [<InlineData("99*76*+.@", "", "123 ")>]
    [<InlineData("&,@", "65 ", "A")>]
    [<InlineData("~.@", "A", "65 ")>]
    [<InlineData("665+*1-,@", "", "A")>]
    [<InlineData("665+*1-.@", "", "65 ")>]
    [<InlineData(">123...@", "", "3 2 1 ")>]
    [<InlineData(">123#...@", "", "3 2 ")>]
    //[<InlineData("v.<\n>:|\n  @", "", "")>]
    [<InlineData("123.$.@", "", "3 1 ")>]
    [<InlineData("123\\...@", "", "2 3 1 ")>]
    [<InlineData("65`.@", "", "1 ")>]
    [<InlineData("25`.@", "", "0 ")>]
    let specExample code input output =
        verify code input output

    [<InlineData("samples-factorial.bf", "1 ", "1 ")>]
    [<InlineData("samples-factorial.bf", "5 ", "120 ")>]
    let sampleFiles file input output = 
        verify (File.ReadAllText file) input output

    [<InlineData("64+\"!dlroW ,olleH\">:#,_@", "", "Hello, World!\n")>]
    [<InlineData("~:1+!#@_,", "this is cat", "this is cat")>]
    let samplePrograms code input output = 
        verify code input output
    
    [<InlineData("01->1# +# :# 0# g# ,# :# 5# 8# *# 4# +# -# _@")>]
    //[<InlineData("0v\n<@_ #! #: #,<*2-1*92,*25,+*92*4*55.0")>]
    //[<InlineData(":0g,:\"~\"`#@_1+0\"Quines are Fun\">_")>]
    let quines q = 
        verify q "" q
