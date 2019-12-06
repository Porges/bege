module Bege.Tests.Optimizer

open Xunit

open Bege.AST
open Bege.Optimizer

[<Fact>]
let ``eventualFate: Exit is a discard`` () =
    Assert.Equal(Discarded, eventualFate { control = Exit; instructions = [] })

[<Fact>]
let ``eventualFate: Branch is a consume`` () =
    Assert.Equal(Consumed, eventualFate { control = Branch 1 1; instructions = [] })

[<Fact>]
let ``eventualFate: Discard is a discard`` () =
    Assert.Equal(Discarded, eventualFate { control = ToState 0; instructions = [Discard] })

