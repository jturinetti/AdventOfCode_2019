using System;
using System.Collections.Generic;
using System.Linq;

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
                
                var allEqualOrGreater = true;
                var currentDigitValue = Convert.ToInt16(numberString[0]);
                var doubleDictionary = new Dictionary<int, int>();

                for (int numberIndex = 1; numberIndex < numberString.Length; numberIndex++)
                {
                    var nextDigitValue = Convert.ToInt16(numberString[numberIndex]);

                    if (nextDigitValue < currentDigitValue)
                    {
                        allEqualOrGreater = false;
                        break;
                    }

                    if (currentDigitValue == nextDigitValue)
                    {                        
                        if (doubleDictionary.ContainsKey(currentDigitValue))
                        {
                            doubleDictionary[currentDigitValue] = doubleDictionary[currentDigitValue] + 1;
                        }
                        else
                        {
                            doubleDictionary.Add(currentDigitValue, 2);
                        }
                    }

                    currentDigitValue = nextDigitValue;
                }

                if (allEqualOrGreater && AdjacentMatchingNumbersMeetConditions(doubleDictionary))
                {
                    passwordCount++;
                }
            }

            return passwordCount;
        }

        private bool AdjacentMatchingNumbersMeetConditions(Dictionary<int, int> doubleDictionary)
        {
            foreach (var key in doubleDictionary.Keys)
            {
                if (doubleDictionary[key] == 2) return true;
            }

            return false;
        }
    }
}