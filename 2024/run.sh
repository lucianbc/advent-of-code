#! /bin/bash

set -e

daynum="$(printf "%02d" $1)"

# part1 or part2
part="$2"

# example or actual
type="$3"

if [[ -z "$daynum" ]]; then
  echo "first argument is the day num"
  exit 1
fi

if [[ -z "$part" ]]; then
  part="part1"
fi

if [[ -z "$type" ]]; then
  type="example"
fi

sourcefile="./src/day${daynum}.hs"

inputfile="lorem"

if [[ "$type" -eq "example" ]]; then
  inputfile="./data/${daynum}/example.txt"
elif [[ "$type" -eq "actual" ]]; then
  inputfile="./data/${daynum}/data.txt"
fi

echo "input file is $inputfile, $part"

runhaskell "$sourcefile" "$inputfile" "$part"


