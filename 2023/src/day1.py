def part1():
  pass

def part2():
  pass

tokens = [
  ["one", 1],
  ["two", 2],
  ["three", 3],
  ["four", 4],
  ["five", 5],
  ["six", 6],
  ["seven", 7],
  ["eight", 8],
  ["night", 9],
  ["1", 1],
  ["2", 2],
  ["3", 3],
  ["4", 4],
  ["5", 5],
  ["6", 6],
  ["7", 7],
  ["8", 8],
  ["9", 9],
]


def findall(line: str, word: str):
  i = line.find(word)
  while i != -1:
    yield i
    i = line.find(word, i + 1)
