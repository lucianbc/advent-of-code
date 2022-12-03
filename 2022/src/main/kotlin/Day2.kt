fun main() {
  Day2.part2()
}



object Day2 {
  fun part1() {
    val input = DataReader.read(2)
    val result = input.sumOf { play(it[0], it[2]) }
    println(result)
  }

  fun part2() {
    val input = DataReader.read(2)
    val result = input.sumOf {
      val yourMapped = mapToPart1(pickYourMove(it[0], it[2]))
      println("their: ${it[0]}, yourMapped: $yourMapped")
      play(it[0], yourMapped)
    }
    println(result)
  }

  fun play(their: Char, your: Char): Int {
    val score = when ((their to your)) {
      ('A' to 'X') -> 3
      ('A' to 'Y') -> 6
      ('A' to 'Z') -> 0
      ('B' to 'X') -> 0
      ('B' to 'Y') -> 3
      ('B' to 'Z') -> 6
      ('C' to 'X') -> 6
      ('C' to 'Y') -> 0
      ('C' to 'Z') -> 3
      else -> 0
    }

    val choiceValue = when (your) {
      'X' -> 1
      'Y' -> 2
      'Z' -> 3
      else -> 0
    }

    return score + choiceValue
  }

  val rock = 'A'
  val paper = 'B'
  val scisor = 'C'

  val win = 'Z'
  val loose = 'X'
  val draw = 'Y'

  fun pickYourMove(their: Char, outcome: Char): Char {
    val score = when ((their to outcome)) {
      (rock to draw) -> rock
      (rock to win) -> paper
      (rock to loose) -> scisor
      (paper to draw) -> paper
      (paper to win) -> scisor
      (paper to loose) -> rock
      (scisor to draw) -> scisor
      (scisor to win) -> rock
      (scisor to loose) -> paper
      else -> 'n'
    }
    return score
  }

  fun mapToPart1(your: Char): Char {
    return when(your) {
      rock -> 'X'
      paper -> 'Y'
      scisor -> 'Z'
      else -> 'x'
    }
  }
}