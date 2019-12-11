module Support

open Xunit.Abstractions

[<AbstractClass>]
type TestBase (output: ITestOutputHelper) =
    let verbose = new System.IO.StringWriter()

    member _.Verbose = verbose :> System.IO.TextWriter

    interface System.IDisposable with
        member _.Dispose() = output.WriteLine(verbose.ToString())
