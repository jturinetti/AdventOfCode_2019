using System;
using System.Collections.Generic;
using System.Linq;

public class Solution
{
    public Wire Wire1 {get;}
    public Wire Wire2 {get;}

    public Solution()
    {
        Wire1 = new Wire();
        Wire2 = new Wire();
    }

    public void ProcessWireInstructions(string[] wire1Instructions, string[] wire2Instructions)
    {
        ProcessSingleWireInstructions(wire1Instructions, Wire1);
        ProcessSingleWireInstructions(wire2Instructions, Wire2);
    }

    private void ProcessSingleWireInstructions(string[] instructions, Wire wire)
    {
        int x = 0;
        int y = 0;

        foreach (var instruction in instructions)
        {
            var direction = instruction[0];
            var length = Convert.ToInt32(instruction.Substring(1));
            switch (direction)
            {
                case 'U':
                    y += length;
                    break;
                case 'D':
                    y -= length;
                    break;
                case 'L':
                    x -= length;
                    break;
                case 'R':
                    x += length;
                    break;
            }

            wire.Coordinates.Add(new Pair(x, y));
        }
    }

    public List<Pair> FindIntersections()
    {
        var intersections = new List<Pair>();

        for (var index = 0; index < Wire1.Coordinates.Count - 1; index++)
        {   
            var w1Coord1 = Wire1.Coordinates[index];
            var w1Coord2 = Wire1.Coordinates[index + 1];

            for (var index2 = 0; index2 < Wire2.Coordinates.Count - 1; index2++)
            {
                var w2Coord1 = Wire2.Coordinates[index2];
                var w2Coord2 = Wire2.Coordinates[index2 + 1];

                // line 1: vertical change, line 2: horizontal change
                if (w1Coord1.X == w1Coord2.X && w2Coord1.Y == w2Coord2.Y)
                {
                    var yAxisCheck = ((w1Coord1.Y
                        > w2Coord1.Y && w1Coord2.Y < w2Coord1.Y) || (w1Coord2.Y > w2Coord1.Y && w1Coord1.Y < w2Coord1.Y));
                    var xAxisCheck = ((w2Coord1.X > w1Coord1.X && w2Coord2.X < w1Coord1.X)
                        || (w2Coord2.X > w1Coord1.X && w2Coord1.X < w1Coord1.X));
                    
                    if (yAxisCheck && xAxisCheck)
                    {
                        // add intersection
                        intersections.Add(new Pair(w1Coord1.X, w2Coord1.Y));
                    }              
                }
                // line 1: horizontal change, line 2: vertical change
                else if (w1Coord1.Y == w1Coord2.Y && w2Coord1.X == w2Coord2.X)
                {
                    // TODO
                }
            }
        }

        return intersections;
    }

    public int FindManhattanDistance()
    {
        // TODO

        return 0;
    }
}