#!/bin/zsh

re='^[0-9]+$'
if ! [[ $1 =~ $re ]] ; then
   echo "error: Not a number" >&2; exit 1
fi

touch "./src/main/kotlin/Day-$1.kt"
touch "./src/main/resources/inputs/day-$1.txt"

TEMPLATE=$(cat <<-END
fun main() {
  Day$1.part1()
}

object Day$1 {
  fun part1() {
    val result = DataReader.read($1)
    println("Result is \$result")
  }

  fun part2() {
    val result = DataReader.read($1)
    println("Result is \$result")
  }
}
END
)

echo "$TEMPLATE" > "./src/main/kotlin/Day-$1.kt"

git add .