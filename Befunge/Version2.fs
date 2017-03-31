// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module V2

open System
open System.IO

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

type [<Struct>] State = { cursor : Cursor; dir : struct (int * int); stack : int list; stringMode : bool }
and Program = char[,]
and [<Struct>] Cursor = { program : Program; row : int; column : int }
    with 
        member c.Read() = c.program.[c.row, c.column]
        member c.Move(struct (x, y)) =
            { c with row = (c.row + x) % 80; column = (c.column + y) % 25 }

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

let step (s : State) : State =
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
            printf "%d " x; s
        | ',' ->
            let struct (x, s) = popped s
            printf "%c" (char x); s
        | '#' -> move s
        | '@' -> raise <| Exception("Done")
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

let initialState p =
    { cursor = { program = p; row = 0; column = 0 } 
    ; dir = right
    ; stack = []
    ; stringMode = false
    }

let run (prog : Program) : unit =

    let rec run' s = run' (move (step s))

    try
        run' (initialState prog)
    with
    | ex -> printfn "%s" ex.Message
        

let main argv = 
    run (parse p2)
    0 // return an integer exit code
