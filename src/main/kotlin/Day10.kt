fun main() {
    part2()
}

private fun part1() {
    val result = read()
        .sorted()
        .let { sequenceOf(0) + it }
        .zipWithNext()
        .map { (small, big) -> big - small }
        .let { it + listOf(3) }
        .groupBy { it }
        .mapValues { it.value.count() }
    println(result)
    println("Answer is ${result[1]!! * result[3]!!}")
}

private fun part2() {
    val data = read().sorted()
        .let { sequenceOf(0) + it }
        .toList()
    val maxes = LongArray(data.size)
    maxes[0] = 1
    for (i in 1 until data.size) {
        var j = i - 1
        var s = 0L
        while (j >= 0 && data[j] >= data[i] - 3) {
            s += maxes[j]
            j--
        }
        maxes[i] = s
    }
    println("All combinations: ${maxes.last()}")
}

private fun read() = DataReader.read(10).asSequence().map { it.toInt() }
