module FungeSpaceTests

open Hedgehog
open Xunit
open System

open Bege

let anyInt = Gen.int (Range.constant Int32.MinValue Int32.MaxValue)

let genIndex = Gen.tuple anyInt

[<Fact>]
let ``can read written value``() =
    let fs = BefungeSpace ()

    Property.check <| property {
        let! (x, y) = genIndex
        let! c = anyInt

        fs.Item(x, y) <- c

        let c' = fs.Item(x, y)
        Assert.Equal(c, c')
    }

    printfn "%A" fs

[<Fact>]
let ``default value is space``() =
    let fs = BefungeSpace ()

    Property.check <| property {
        let! (x, y) = genIndex
        let c = fs.Item(x, y)

        Assert.Equal(int ' ', c)
    }

[<Fact>]
let ``writing a second value does not affect the first``() =
    let fs = BefungeSpace ()

    Property.check <| property {
        let! (x, y) = genIndex
        let! c = anyInt
        fs.Item(x, y) <- c

        let! (x', y') = genIndex
        where (x' <> x || y' <> y) // ensure no clashes
        let! c' = anyInt
        fs.Item(x', y') <- c'
        
        let origC = fs.Item(x, y) 
        Assert.Equal(c, origC)
    }

[<Fact>]
let ``can write a 256x256 block and read it back`` () =
    let fs = BefungeSpace ()

    let value i j = i * 256 + j

    for i in 0..255 do
        for j in 0..255 do
            fs.[i, j] <- value i j

    for i in 0..255 do
        for j in 0..255 do
            Assert.Equal(value i j, fs.[i, j])