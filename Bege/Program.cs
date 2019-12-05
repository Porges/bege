using Microsoft.FSharp.Core;
using System;
using System.ComponentModel.DataAnnotations;
using System.IO;
using static Bege.Common;

namespace Bege
{
    static class Program
    {
        /// <param name="input">The input file (e.g. "example.bf")</param>
        /// <param name="optimize">Optimize program</param>
        /// <param name="std">The Funge standard to use (e.g. "befunge93", "befunge98")</param>
        /// <param name="verbose">Verbose output</param>
        static int Main(
            [Required] FileInfo input,
            bool optimize = true,
            string std = "befunge98",
            bool verbose = false)
        {
            string ReadProgramText()
            {
                if (input != null)
                {
                    try
                    {
                        return File.ReadAllText(input.FullName);
                    }
                    catch (FileNotFoundException ex)
                    {
                        throw new FatalException($"File does not exist: '{input.FullName}'.", ExitCodes.InputNotFound, ex);
                    }
                }
                else
                {
                    Console.Error.WriteLine("Reading program from stdin...");
                    return Console.In.ReadToEnd();
                }
            }

            try
            {
                var standard = Options.Standard.TryParse(std);
                if (standard == FSharpOption<Options.Standard>.None)
                {
                    throw new FatalException($"Unsupported standard '{std}'.", ExitCodes.StandardNotSupported);
                }

                var factory = Compiler.compile(
                    new Options.Options(standard.Value, optimize, verbose),
                    ReadProgramText());

                var funge = factory.create(Console.In, Console.Out, (ulong)Guid.NewGuid().GetHashCode());

                funge.Run();

                return 0;
            }
            catch (FatalException ex)
            {
                Console.Error.WriteLine(ex.Message);
                return ex.ExitCode;
            }
        }
    }
}
