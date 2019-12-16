using System;
using System.IO;

namespace day3
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] wire1Instructions, wire2Instructions;

            using (StreamReader fileStream = new StreamReader(new FileInfo("input.txt").OpenRead()))
            {
                var wire1Line = fileStream.ReadLine();
                var wire2Line = fileStream.ReadLine();

                wire1Instructions = wire1Line.Split(',');
                wire2Instructions = wire2Line.Split(',');
            }

            var solution = new Solution();
            solution.ProcessWireInstructions(wire1Instructions, wire2Instructions);
            solution.FindIntersections();
            var result = solution.FindManhattanDistance();

            Console.WriteLine("Answer: " + result);
            Console.ReadLine();
        }
    }
}
