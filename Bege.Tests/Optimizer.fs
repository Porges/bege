namespace Bege.Tests

open System.Linq
open Xunit

open Bege
open Bege.Parser
open Bege.Options
open Bege.Optimizer

type Optimizer (itoh: Xunit.Abstractions.ITestOutputHelper) = 
    inherit Support.TestBase(itoh)
    let verbose = base.Verbose

    let start = 0

    [<Fact>]
    let ``eventualFate: Exit is a discard`` () =
        Assert.Equal(Discarded, eventualFate { start=start; control = Exit; instructions = [] })

    [<Fact>]
    let ``eventualFate: Branch is a consume`` () =
        Assert.Equal(Consumed, eventualFate { start=start; control = Branch 1 1; instructions = [] })

    [<Fact>]
    let ``eventualFate: Discard is a discard`` () =
        Assert.Equal(Discarded, eventualFate { start=start; control = ToState 0; instructions = [Discard] })

    [<Fact>]
    let ``eventualFate: OutputNumber is a consume`` () =
        Assert.Equal(Consumed, eventualFate { start=start; control = ToState 0; instructions = [OutputNumber] })
        
    [<Fact>]
    let ``eventualFate: Discard is detected after several instructions`` () =
        Assert.Equal(Discarded, eventualFate { start=start; control = ToState 0; instructions = [InputNumber; Discard; Discard] })

    [<Fact>]
    let ``eventualFate: Consume is detected after several instructions`` () =
        Assert.Equal(Consumed, eventualFate { start=start; control = ToState 0; instructions = [InputNumber; Discard; OutputNumber] })

        
    [<Fact>]
    let ``eventualFate: slightly more complex`` () =
        Assert.Equal(Discarded, eventualFate { start=start; control = ToState 0; instructions = [InputNumber; InputNumber; BinOp Add; OutputNumber; Discard] })


    let options = { verbose = verbose; optimize = true; standard = Standard.Befunge98 }
    let parsed (txt: string): Program<_> = 
        Loader.load txt |> Parser.parse options


    //[<Fact>]
    //let ``identical chains are collapsed`` () =
        
    //    let program = parsed "1>v<<\n@.?.@\n  .  \n  @  " |> Optimizer.toTypedProgram options

    //    let p1 = program
    //    let p2 = collapseIdenticalChains program

    //    // reduces by 2 since there are 3 identical chains
    //    Assert.Equal(7, p1.Count)
    //    Assert.Equal(4, p2.Count) 

    //[<Fact>]
    //let ``check fully optimized version`` () =

    //    let opt = "1>v<<\n@.?.@\n  .  \n  @  " |> Loader.load |> Parser.parse options |> optimize options

    //    let only = Assert.Single opt
    //    Assert.Equal<_ list>([Load 1; OutputNumber], only.Value.chain.instructions)
