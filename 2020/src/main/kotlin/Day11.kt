fun main() {
//    Tests.test1()
    part2()
}

private fun part1() {
    runSim(::simulateNext)
}

private fun part2() {
    runSim(::simulateNext2)
}

private fun runSim(simulation: (SeatMap) -> SeatMap) {
    val data = DataReader
        .read(11)
        .map { it.toCharArray() }
        .toTypedArray()
    
    var map = data
    var next = simulation(map)
//    next = simulation(next)
//    println(next.show())
    while (!map.sameAs(next)) {
        map = next
        next = simulation(map)
    }
    val empty = map.flatMap { it.asSequence() }.count { it == '#' }
    println(empty)
}


private fun SeatMap.show(): String {
    return this.joinToString(separator = "") { it.joinToString(separator = "", postfix = "\n") }
}

private fun SeatMap.sameAs(other: SeatMap): Boolean {
    return this.asSequence()
        .flatMap { it.asSequence() }
        .zip(other.asSequence().flatMap { it.asSequence() })
        .fold(true) { acc, (first, second) -> acc && first == second }
}

private fun SeatMap.countOccupied(line: Int, col: Int) = directions
    .map { (line to col) + it }
    .filter { this.contains(it) }
    .map {
        this[it.first][it.second]
    }
    .count { it == '#' }

private fun SeatMap.countOccupiedInSight(line: Int, col: Int): Int {
    var occupied = 0
    for (d in directions) {
        var x = 1
        var point = x * d + (line to col)
        while (this.contains(point) && this[point.first][point.second] == '.') {
            x += 1
            point = x * d + (line to col)
        }
        if (this.contains(point) && this[point.first][point.second] == '#') {
            occupied += 1
        }
    }
    return occupied
}

private fun simulateParameterized(map: SeatMap, occThreshold: Int, occCount: (SeatMap, Int, Int) -> Int): SeatMap {
    val newMap = Array(map.size) { CharArray(map[0].size) }
    for (line in map.indices) {
        for (col in map[line].indices) {
            val occupiedCount = occCount(map, line, col)
            if (map[line][col] == 'L' && occupiedCount == 0) {
                newMap[line][col] = '#'
            } else if (map[line][col] == '#' && occupiedCount >= occThreshold) {
                newMap[line][col] = 'L'
            } else {
                newMap[line][col] = map[line][col]
            }
        }
    }
    return newMap
}

private fun simulateNext(map: SeatMap): SeatMap =
    simulateParameterized(map, 4, SeatMap::countOccupied)

private fun simulateNext2(map: SeatMap): SeatMap =
    simulateParameterized(map, 5, SeatMap::countOccupiedInSight)

private val directions =
    listOf(-1 to -1, -1 to 0, -1 to 1, 0 to -1, 0 to 1, 1 to -1, 1 to 0, 1 to 1)

private operator fun Pair<Int, Int>.plus(other: Pair<Int, Int>): Pair<Int, Int> {
    return this.first + other.first to this.second + other.second
}

private operator fun Int.times(other: Pair<Int, Int>): Pair<Int, Int> {
    return this * other.first to this * other.second
}

private operator fun Pair<Int, Int>.plus(other: Int): Pair<Int, Int> {
    return this.first + other to this.second + other
}

private operator fun SeatMap.contains(point: Pair<Int, Int>): Boolean =
    point.first >= 0 && point.second >= 0 && point.first < this.size && point.second < this[0].size

typealias SeatMap = Array<CharArray>

private fun read(): SeatMap = DataReader
    .read(11)
    .toMap()

private fun List<String>.toMap() = this
    .map { it.toCharArray() }
    .toTypedArray()

object Tests {
    fun test1() {
        val input =
            ".......#.\n" +
            "...#.....\n" +
            ".#.......\n" +
            ".........\n" +
            "..#L....#\n" +
            "....#....\n" +
            ".........\n" +
            "#........\n" +
            "...#....."
        
        val m = input.split("\n").toMap()
        
        val x = m.countOccupiedInSight(4, 3)
        
        println(x) // should be 8
    }
}
