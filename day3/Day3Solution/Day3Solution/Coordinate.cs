namespace Day3Solution
{
    public class Coordinate    
    {
        public Coordinate(int x, int y)
        {
            X = x;
            Y = y;
        }

        public int X {get;}
        public int Y {get;}

        public int StepsTaken {get;set;}
    }
}