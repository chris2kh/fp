puzzle = bytearray((int(c) for c in '864801'))
recipes_found = bytearray([3, 7])

elf1, elf2 = 0, 1
loop = 0
while True:
    if (loop % 250_000) == 0 and puzzle in recipes_found:
        break
    recipe1, recipe2 = recipes_found[elf1], recipes_found[elf2]
    new_recipe = recipe1 + recipe2
    digit1, digit2 = new_recipe // 10, new_recipe % 10
    if digit1 ^ 0:
        recipes_found.append(digit1)
    recipes_found.append(digit2)
    elf1 = (elf1 + 1 + recipe1) % len(recipes_found)
    elf2 = (elf2 + 1 + recipe2) % len(recipes_found)
    loop += 1

print(recipes_found.index(puzzle))
