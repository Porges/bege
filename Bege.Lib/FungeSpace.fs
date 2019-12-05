module Bege.FungeSpace

(*
 Befunge 98 space is (int32, int32)-indexed space.

 We instantiate a block at a time to try to increase locality and reduce the
 number of entries, rather than one dictionary entry per memory cell.

 The blocks are 0x3F * 0x3F in size, giving just under 4000 cells per block.
 This was chosen to be on the order of the Befunge-93 space of 80x25 = 2000 cells.
*)
type Funge98Space() =
    let storage = System.Collections.Generic.Dictionary<int64, int32[]>()

    let bitMask = 0x3F
    let blockSize = (bitMask + 1) * (bitMask + 1)
    let notMask = ~~~0x3F
    let ix x y = (int64(x &&& notMask) <<< 32) ||| int64(y &&& notMask)
    let subIx x y = ((x &&& bitMask) <<< 6) ||| (y &&& bitMask)

    // Cache the last-used block to avoid dict lookups:
    let mutable lastIx : int64 = 0L
    let mutable lastArr : int32[] = null

    let defaultValue = int ' '

    let newBlock ix =
        let result = Array.create blockSize defaultValue
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
                | (false, _) -> defaultValue

        and set(x : int32, y : int32) (c : int32) =
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
