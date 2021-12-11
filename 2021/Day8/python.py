from itertools import combinations

with open("input.txt", "rt") as file:
    values = []
    for line in file:
        inp, out = line.rstrip().split(" | ")
        values += [(inp.split(), out.split())]

# Part 1

count = 0
for (inp, out) in values:
    for value in out:
        size = len(value)
        if size in (2, 4, 3, 7): count += 1

print(f"Part 1: {count}")
