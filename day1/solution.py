def calculate_fuel(str_num):
    return int(int(str_num) / 3) - 2

def calculate_fuel_requirements():
    input_file = open("input.txt", "r")
    file_content = input_file.readlines()
    return sum(map(calculate_fuel, file_content))

print(calculate_fuel_requirements())