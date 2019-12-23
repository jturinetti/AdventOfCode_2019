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
            using (StreamReader fileStream = new StreamReader(new FileInfo("input.txt").OpenRead()))
            {
                while (!fileStream.EndOfStream)
                {
                    solution.ParseInputLine(fileStream.ReadLine());
                }
            }

            // calculate total orbits
            var result = solution.CalculateOrbits();

            Console.WriteLine(result);
        }
    }
}
