module Bege.InstructionPointer

/// Instruction pointer direction
type Delta = (struct (int * int))
type Position = (struct (int * int))
// (Important that Right is first so the initial instruction pointer
//  sorts before others at the same position...) - TODO: fix this again

let reflect (struct (x, y)) = struct (-x, -y)

module Dir =
    let right = struct (0, 1)
    let left = struct (0, -1)
    let up = struct (-1, 0)
    let down = struct (1, 0)

/// Instruction pointer state
[<StructuredFormatDisplay("{dir} {position}")>]
type [<Struct>] IPState = { position : Position; delta : Delta }

/// Initial pointer state
let programEntryState = { position = struct (0,0); delta = Dir.right }

let advance (ip : IPState) = 
    let struct (x, y) = ip.position
    let struct (dx, dy) = ip.delta

    let x' =
        let x = x + dx in
        if x < 0 then x + 25 else x % 25

    let y' = 
        let y = y + dy in
        if y < 0 then y + 80 else y % 80

    { ip with position = struct (x', y') }