﻿using System;
using System.Collections.Generic;
using System.IO;

namespace Day3Solution
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
            var intersections = solution.FindIntersections();
            var manhattanDistance = solution.FindManhattanDistance(intersections);
            var minimizedSignalDelay = solution.FindMinimalSignalDelay(intersections);

            Console.WriteLine("Manhattan Distance: " + manhattanDistance);
            Console.WriteLine("Minimized Signal Delay: " + minimizedSignalDelay);
        }
    }
}
