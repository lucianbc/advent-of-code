fun main() {
  Day1.execute()
}

object Day1 {
  fun execute() {
    val input = DataReader.read(1)

    val splits = input.splitBy("")
    val calories = splits.map {
      it.sumOf { x -> x.toInt() }
    }.sortedDescending()

    println(calories)

    val part1 = calories.first()

    val part2 = calories.take(2).sum()

    println(part1) // 66616
    println(part1 + part2) // 199172
  }
}


fun <T> List<T>.splitBy(element: T): Sequence<List<T>> {
  var crtBlock = mutableListOf<T>()
  if (this.isEmpty()) {
    return emptySequence()
  }
  return sequence<List<T>> {
    this@splitBy.forEach {
      if (it == element) {
        if (crtBlock.isNotEmpty()) {
          yield(crtBlock)
        }
        crtBlock = mutableListOf()
      } else {
        crtBlock.add(it)
      }
    }
    if (crtBlock.isNotEmpty()) {
      yield(crtBlock)
    }
  }
}