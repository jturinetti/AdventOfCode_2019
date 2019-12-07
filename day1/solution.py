def calculate_fuel(num):
    return int(num / 3) - 2

def calculate_fuel_recursive(num):
    current_fuel_calc = calculate_fuel(num)

    if current_fuel_calc <= 0:
        return 0

    return current_fuel_calc + calculate_fuel_recursive(current_fuel_calc)

def convert_to_int(str_num):
    return int(str_num)

def calculate_fuel_requirements(map_func):
    input_file = open("input.txt", "r")
    file_content = map(convert_to_int, input_file.readlines())
    return sum(map(map_func, file_content))

print(calculate_fuel_requirements(calculate_fuel))
print(calculate_fuel_requirements(calculate_fuel_recursive))