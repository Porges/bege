module Bege.Tests

open Xunit
open System.IO

open Bege.Compiler
open Bege.Options

let private verify code input output =

    for optimize in [false; true] do
        let options =
            { outputFileName = None
            ; optimize = optimize
            ; standard = befunge98
            }

        use inS = new StringReader(input)
        use outS = new StringWriter()
        let count = run options 0UL inS outS code

        Assert.Equal(output, outS.ToString())

let private verifyMode year code input output =

    for optimize in [false; true] do
        let options =
            { outputFileName = None
            ; optimize = optimize
            ; standard = { befunge98 with year = year }
            }

        use inS = new StringReader(input)
        use outS = new StringWriter()
        let count = run options 0UL inS outS code

        Assert.Equal(output, outS.ToString())

let private verifyOptimized code input output expectedInsns =

    let options =
        { outputFileName = None
        ; optimize = true
        ; standard = befunge98
        }

    use inS = new StringReader(input)
    use outS = new StringWriter()
    let count = run options 0UL inS outS code

    Assert.Equal(output, outS.ToString()) 
    Assert.Equal(expectedInsns, count)

(* Examples from the GitHub spec *)
[<Theory>] 
[<InlineData("@", "", "", 0)>]
[<InlineData("99*76*+.@", "", "123 ", 1)>]
[<InlineData("&,@", "65 ", "A", 2)>]
[<InlineData("~.@", "A", "65 ", 2)>]
[<InlineData("665+*1-,@", "", "A", 1)>]
[<InlineData("665+*1-.@", "", "65 ", 1)>]
[<InlineData(">123...@", "", "3 2 1 ", 7)>]
[<InlineData(">123#...@", "", "3 2 ", 5)>]
[<InlineData("123.$.@", "", "3 1 ", 6)>]
[<InlineData("123\\...@", "", "2 3 1 ", 9)>]
[<InlineData("65`.@", "", "1 ", 1)>]
[<InlineData("25`.@", "", "0 ", 1)>]
let specExample code input output insns =
    verify code input output
    verifyOptimized code input output insns

[<Fact>]
let ``strings are pushed backward``() =
    verify "\"ver\",,,@" "" "rev"

[<Fact>]
let ``dup works``() =
    verify "1:..@", "", "1 1 "

[<Fact>]
let ``read dup write write`` () =
    verify "~:,,@" "A" "AA"

[<Fact>]
let ``popping empty stack produces zero``() =
    verify "1..@" "" "1 0 "

[<Fact>]
let ``not 1 is 0``() =
    verify "1!.@" "" "0 "

[<Fact>]
let ``not 2 is 0``() =
    verify "2!.@" "" "0 "

[<Fact>]
let ``not 0 is 1``() =
    verify "0!.@" "" "1 "

[<Fact>]
let ``can wrap around from first position`` () =
    verify "<@,~" "A" "A"

[<Fact>]
let ``optimized branch still pops`` ()  =
    verifyOptimized "12v\n@._.@" "" "1 " 1

[<Fact>]
let ``98 string mode collapses multiple spaces`` () =
    verifyMode 98 "\"z  a\",,,@" "" "a z"

[<Fact>]
let ``93 string mode preserves multiple spaces`` () =
    verifyMode 93 "\"z  a\",,,,@" "" "a  z"

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
// Hello worlds:
[<InlineData("64+\"!dlroW ,olleH\">:#,_@", "", "Hello, World!\n")>]
[<InlineData("<>>#;>:#,_@;\"Hello, world!\"", "", "Hello, world!")>]
[<InlineData(";Hello, world!; >00ga6*1-->#@_1>:#<>#<0#<g#<:#<a#<6#<*#<1#<-#<-#<>#+>#1>#,_$$@", "", "Hello, world!")>]

// cat:
[<InlineData("~:1+!#@_,", "this is cat", "this is cat")>]

// factorial: 
// [<InlineData("5 :>>#;1-:48*+01p*01g48*-#;1#+-#1:#<_$.@", "", "120 ")>]
// uses 'p', not supported
let samplePrograms code input output = 
    verify code input output

[<Theory>]
[<InlineData("01->1# +# :# 0# g# ,# :# 5# 8# *# 4# +# -# _@")>]
//[<InlineData("0v\n\"<@_ #! #: #,<*2-1*92,*25,+*92*4*55.0")>]
// [<InlineData(":0g,:\"~\"`#@_1+0\"Quines are Fun\">_")>]
// ^ inserts extraneous spaces
let quine q = 
    verify q "" q
