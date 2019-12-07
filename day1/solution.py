class Day1Solution():
    def calculate_fuel(self, num):
        return int(num / 3) - 2

    def calculate_fuel_recursive(self, num):
        current_fuel_calc = self.calculate_fuel(num)

        if current_fuel_calc <= 0:
            return 0

        return current_fuel_calc + self.calculate_fuel_recursive(current_fuel_calc)

    def calculate_fuel_requirements(self, map_func):
        input_file = open("input.txt", "r")
        file_content = map(lambda x: int(x), input_file.readlines())
        return sum(map(map_func, file_content))

solution = Day1Solution()

print(solution.calculate_fuel_requirements(solution.calculate_fuel))
print(solution.calculate_fuel_requirements(solution.calculate_fuel_recursive))