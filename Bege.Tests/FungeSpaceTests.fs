module FungeSpaceTests

open Hedgehog
open Xunit

open Bege.FungeSpace

let genIndex = Gen.tuple Gen.int

[<Fact>]
let ``can read written value``() =
    let fs = Funge98Space()

    Property.check <| property {
        let! (x, y) = genIndex
        let! c = Gen.int

        fs.Item(x, y) <- c

        let c' = fs.Item(x, y)
        Assert.Equal(c, c')
    }

    printfn "%A" fs

[<Fact>]
let ``default value is space``() =
    let fs = Funge98Space()

    Property.check <| property {
        let! (x, y) = genIndex
        let c = fs.Item(x, y)

        Assert.Equal(int ' ', c)
    }

[<Fact>]
let ``writing a second value does not affect the first``() =
    let fs = Funge98Space()

    Property.check <| property {
        let! (x, y) = genIndex
        let! c = Gen.int
        fs.Item(x, y) <- c

        let! (x', y') = genIndex
        where (x' <> x || y' <> y) // ensure no clashes
        let! c' = Gen.int
        fs.Item(x', y') <- c'
        
        let origC = fs.Item(x, y) 
        Assert.Equal(c, origC)
    }

[<Fact>]
let ``can write a 256x256 block and read it back`` () =
    let fs = Funge98Space()

    let value i j = i * 256 + j

    for i in 0..255 do
        for j in 0..255 do
            fs.[i, j] <- value i j

    for i in 0..255 do
        for j in 0..255 do
            Assert.Equal(value i j, fs.[i, j])