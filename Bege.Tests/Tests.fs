module Bege.Tests

open Xunit
open System.IO

open Bege.Compiler

let private verify code input output =
    use inS = new StringReader(input)
    use outS = new StringWriter()
    let count = run (Bege.Parser.parse code) 0UL inS outS

    Assert.Equal(output, outS.ToString()) 

let private verifyOptimized code input output expectedInsns =
    use inS = new StringReader(input)
    use outS = new StringWriter()
    let count = run (Bege.Parser.parse code) 0UL inS outS

    Assert.Equal(output, outS.ToString()) 
    Assert.Equal(expectedInsns, count)

(* Examples from the GitHub spec *)
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

let ``strings are pushed backward``() =
    verify "\"ver\",,,@" "" "rev"

let ``dup works``() =
    verify "1:..@", "", "1 1 "

let ``read dup write write`` () =
    verify "~:,,@" "A" "AA"

let ``popping empty stack produces zero``() =
    verify "1..@" "" "1 0 "

let ``not 1 is 0``() =
    verify "1!.@" "" "0 "

let ``not 2 is 0``() =
    verify "2!.@" "" "0 "

let ``not 0 is 1``() =
    verify "1!.@" "" "1 "

let ``can wrap around from first position`` () =
    verify "<@,~" "A" "A"

[<Theory>]
[<InlineData("samples-factorial.bf", "1 ", "1 ")>]
[<InlineData("samples-factorial.bf", "2 ", "2 ")>]
[<InlineData("samples-factorial.bf", "3 ", "6 ")>]
[<InlineData("samples-factorial.bf", "5 ", "120 ")>]
// [<InlineData("samples-sieve.bf", "10 ", "2 ")>]
// uses 'p', not supported
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
let quine q = 
    verify q "" q
