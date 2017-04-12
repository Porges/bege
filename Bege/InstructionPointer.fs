module Bege.InstructionPointer

/// Instruction pointer direction
type Dir = Right = 0 | Up = 1 | Down = 2 | Left = 3
// (Important that Right is first so the initial instruction pointer
//  sorts before others at the same position...)

let reflect = function
    | Dir.Right -> Dir.Left
    | Dir.Up -> Dir.Down
    | Dir.Left -> Dir.Right
    | Dir.Down -> Dir.Up

/// Instruction pointer state
[<StructuredFormatDisplay("{dir} {position}")>]
type [<Struct>] IPState = { position : struct (int * int) ; dir : Dir }

/// Initial pointer state
let programEntryState = { position = struct (0,0); dir = Dir.Right }

let advance (ip : IPState) = 
    let struct (x, y) = ip.position
    let (x', y') =
        match ip.dir with
        | Dir.Right -> (x, y+1)
        | Dir.Left -> (x, y-1)
        | Dir.Up -> (x-1, y)
        | Dir.Down -> (x+1, y)
    let x' = if x' < 0 then x' + 25 else x' % 25
    let y' = if y' < 0 then y' + 80 else y' % 80
    { ip with position = struct (x', y') }