using System;
using System.IO;

namespace Day6Solution
{
    class Program
    {
        static void Main(string[] args)
        {
            var solution = new Solution();

            // read input and create data structure
            using (StreamReader fileStream = new StreamReader(new FileInfo(args[0]).OpenRead()))
            {
                while (!fileStream.EndOfStream)
                {
                    solution.ParseInputLine(fileStream.ReadLine());
                }
            }

            // calculate total orbits
            var result = solution.CalculateOrbits();

            Console.WriteLine();
            Console.WriteLine($"Total Orbits: {result}");

            // calculate YOU to SAN
            var result2 = solution.CalculateYOUToSANOrbits();

            Console.WriteLine();
            Console.WriteLine($"YOU to SAN Orbits: {result2}");

            Console.WriteLine();
        }
    }
}
