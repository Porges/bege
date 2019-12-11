using System;
using System.ComponentModel.DataAnnotations;
using System.IO;

using Bege.Options;

namespace Bege
{
    static class Program
    {
        /// <param name="input">The input file (e.g. "example.bf")</param>
        /// <param name="optimize">Optimize program</param>
        /// <param name="std">The Funge standard to use (e.g. "befunge93", "befunge98")</param>
        /// <param name="verbose">Verbose output</param>
        static int Main(
            FileInfo input,
            bool optimize = true,
            string std = "befunge98",
            bool verbose = false)
        {
            string ReadProgramText()
            {
                if (input != null)
                {
                    throw new FatalException($"No input file specified", ExitCodes.InvalidOptions);
                }

                try
                {
                    return File.ReadAllText(input.FullName);
                }
                catch (FileNotFoundException ex)
                {
                    throw new FatalException($"File does not exist: '{input.FullName}'.", ExitCodes.InputNotFound, ex);
                }
            }

            try
            {
                if (!Standard.TryParse(std, out var standard))
                {
                    throw new FatalException($"Unsupported standard '{std}'.", ExitCodes.StandardNotSupported);
                }

                var factory = Compiler.Compile(
                    new Options.Options(standard, optimize, verbose ? Console.Error : new StringWriter()), // TODO: NullWriter instead of StringWriter
                    ReadProgramText());

                var funge = factory.Create(Console.In, Console.Out, (ulong)Guid.NewGuid().GetHashCode());

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
