import unittest
import solution as SolutionFile

class Day1UnitTests(unittest.TestCase):
    def setUp(self):
        self.testObj = SolutionFile.Day1Solution()

    def test_problem1_fuel_calc(self):
        result = self.testObj.calculate_fuel(12)
        self.assertEqual(result, 2)

        result = self.testObj.calculate_fuel(14)
        self.assertEqual(result, 2)

        result = self.testObj.calculate_fuel(1969)
        self.assertEqual(result, 654)

        result = self.testObj.calculate_fuel(100756)
        self.assertEqual(result, 33583)

    def test_problem2_fuel_calc(self):
        result = self.testObj.calculate_fuel_recursive(14)
        self.assertEqual(result, 2)

        result = self.testObj.calculate_fuel_recursive(1969)
        self.assertEqual(result, 966)

        result = self.testObj.calculate_fuel_recursive(100756)
        self.assertEqual(result, 50346)


if __name__ == '__main__':
    unittest.main()
