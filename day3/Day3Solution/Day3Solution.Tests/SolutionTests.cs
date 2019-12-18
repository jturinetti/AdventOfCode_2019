using System;
using System.Collections.Generic;
using Xunit;

namespace Day3Solution.Tests
{
    public class SolutionTests
    {
        [Fact]
        public void Solution_Example1_ReturnsCorrectResult()
        {
            var sol = new Solution();
            var wire1 = new List<string> { "R8", "U5", "L5", "D3" };
            var wire2 = new List<string> { "U7", "R6", "D4", "L4" };

            sol.ProcessWireInstructions(wire1.ToArray(), wire2.ToArray());
            var intersections = sol.FindIntersections();
            var result = sol.FindManhattanDistance(intersections);

            Assert.Equal(2, intersections.Count);
            Assert.Equal(6, result);
        }

        [Fact]
        public void Solution_Example2_ReturnsCorrectResult()
        {
            var sol = new Solution();
            var wire1 = new List<string> { "R75", "D30" ,"R83", "U83", "L12", "D49", "R71", "U7", "L72" };
            var wire2 = new List<string> { "U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83" };

            sol.ProcessWireInstructions(wire1.ToArray(), wire2.ToArray());
            var intersections = sol.FindIntersections();
            var result = sol.FindManhattanDistance(intersections);

            Assert.Equal(159, result);
        }

        [Fact]
        public void Solution_Example3_ReturnsCorrectResult()
        {
            var sol = new Solution();
            var wire1 = new List<string> { "R98", "U47", "R26", "D63", "R33", "U87", "L62", "D20", "R33", "U53", "R51" };
            var wire2 = new List<string> { "U98", "R91", "D20", "R16", "D67", "R40", "U7", "R15", "U6", "R7" };

            sol.ProcessWireInstructions(wire1.ToArray(), wire2.ToArray());
            var intersections = sol.FindIntersections();
            var result = sol.FindManhattanDistance(intersections);

            Assert.Equal(135, result);
        }
    }
}
