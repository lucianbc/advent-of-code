fun main() {
  Day3.part2()
}

object Day3 {
  fun part1() {
    val codes = DataReader.read(3)
      .map { getOverlap(it).let(::mapChar) }
      .sum()

    println(codes)
  }

  fun part2() {
    DataReader.read(3)
      .groupIn(3)
      .apply {
        println(this)
      }
      .map { intersect(*it.toTypedArray()) }
      .map { mapChar(it) }
      .sum()
      .let { println(it) }
  }

  fun <T> List<T>.groupIn(count: Int): List<List<T>> {
    var group = mutableListOf<T>()
    val result = mutableListOf<List<T>>()

    this.forEach {
      if (group.size == count) {
        result.add(group)
        group = mutableListOf()
        group.add(it)
      } else {
        group.add(it)
      }
    }

    if (group.isNotEmpty()) {
      result.add(group)
    }

    return result
  }

  fun intersect(vararg strings: String): Char {
    val intersection = strings
      .asSequence()
      .fold(strings[0].toSet()) { acc, crt ->
        acc.intersect(crt.toSet())
      }
    return intersection.first()
  }

  fun getOverlap(rucksack: String): Char {
    val comp1 = rucksack.subSequence(0, rucksack.length / 2)
    val comp2 = rucksack.subSequence(rucksack.length / 2, rucksack.length)

    // val intersection = comp1.toSet().intersect(comp2.toSet())
    val intersection = intersect(comp1.toString(), comp2.toString())
    return intersection
  }

  fun mapChar(char: Char): Int {
    return if (char.isUpperCase()) {
      27 + char.code - 'A'.code
    } else {
      char.code - 'a'.code + 1
    }
  }
}