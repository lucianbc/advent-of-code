import java.util.*

fun main() {
  Day6.part2()
}

object Day6 {
  fun part1() {
    val buffer = DataReader.read(6)[0]
    val window = LinkedList<Char>()
    var scannedChars = 0

    val iterator = buffer.iterator()

    while (iterator.hasNext()) {
      val crtChar = iterator.nextChar()
      scannedChars++
      if (window.size == 4) {
        window.removeFirst()
      }

      window.addLast(crtChar)

      if (window.size == 4 && window.allDifferent()) {
        break
      }
    }

    println("Result is $scannedChars")
  }

  fun List<Char>.allDifferent(): Boolean {
    val s = this.toSet()
    return s.size == this.size
  }

  fun part2() {
    val buffer = DataReader.read(6)[0]
    val window = LinkedList<Char>()
    var scannedChars = 0

    val iterator = buffer.iterator()

    while (iterator.hasNext()) {
      val crtChar = iterator.nextChar()
      scannedChars++
      if (window.size == 14) {
        window.removeFirst()
      }

      window.addLast(crtChar)

      if (window.size == 14 && window.allDifferent()) {
        break
      }
    }

    println("Result is $scannedChars")
  }
}
