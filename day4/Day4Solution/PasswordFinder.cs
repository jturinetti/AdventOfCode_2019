using System;

namespace Day4Solution
{
    public class Solution
    {
        public int LowerBound {get;} = 171309;
        public int UpperBound {get;} = 643603;

        public int CountPossiblePasswords()
        {
            var passwordCount = 0;

            for (int currentNumber = LowerBound; currentNumber <= UpperBound; currentNumber++)
            {
                var numberString = currentNumber.ToString();
                if (numberString.Contains("0"))
                {
                    continue;
                }

                var doubleFound = false;
                var allEqualOrGreater = true;
                var currentDigitValue = Convert.ToInt16(numberString[0]);

                for (int numberIndex = 1; numberIndex < numberString.Length; numberIndex++)
                {
                    var nextDigitValue = Convert.ToInt16(numberString[numberIndex]);
                    if (currentDigitValue == nextDigitValue)
                    {
                        doubleFound = true;
                    }
                    else if (nextDigitValue < currentDigitValue)
                    {
                        allEqualOrGreater = false;
                        break;
                    }

                    currentDigitValue = nextDigitValue;
                }

                if (doubleFound && allEqualOrGreater)
                {
                    passwordCount++;
                }
            }

            return passwordCount;
        }
    }
}