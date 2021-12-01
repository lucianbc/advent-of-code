data class Slope (val down: Int, val right: Int)

fun main() {
    println("Hello, world")
    
    part2()
}

private fun solveOne(data: List<String>, slope: Slope) =
    data.asSequence().step(slope.down).fold(0 to 0) { (trees, crtPlace), line ->
        (if (line[crtPlace] == '#') trees + 1 else trees) to (crtPlace + slope.right) % data[0].length
    }.first

private fun part1() {
    val slope = Slope(1, 3)
    
    val data = DataReader.read(3)
    
    val result = solveOne(data, slope)
    
    println("Result here: $result")
}

private fun part2Naive() {
    val data = DataReader.read(3)
    
    val slopes0 = listOf(
        Slope(1, 1),
        Slope(1, 3),
        Slope(1, 5),
        Slope(1, 7),
        Slope(2, 1)
    )
    
    val ress = slopes0.map { solveOne(data, it) }
    
    println(ress)
    
    println(ress.fold(1L) {it, acc -> it * acc})
}

private fun part2() {
    val data = DataReader.read(3)
    
    val slopes0 = listOf(
        Slope(1, 1),
        Slope(1, 3),
        Slope(1, 5),
        Slope(1, 7),
        Slope(2, 1)
    ).map { it to (0 to 0) }
    
    val result = data.asSequence().withIndex().fold(slopes0) { slopes, (index, line) ->
        slopes.map {
            val (trees, crtPlace) = it.second
            val slope = it.first
            if (index % slope.down != 0) it
            else slope to ((trees + if (line[crtPlace] == '#') 1 else 0) to (crtPlace + slope.right) % data[0].length)
        }
    }.fold(1L) { acc, crt -> acc * crt.second.first}
    
    println("Result here: ${result}")
}

fun <T>Sequence<T>.step(s: Int): Sequence<T> {
    return this.withIndex().filter { it.index % s == 0 }.map { it.value }
}
