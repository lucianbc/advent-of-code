
fun main() {
    part2()
}

private fun part1() {
    val first = DataReader.read(5)
        .sortedWith { c1, c2 -> compareSeats(c1, c2) }
        .last()
    
    println(first)
    println(Seat.compute(first))
}

private fun part2() {
    val ids = DataReader.read(5)
        .map { Seat.compute(it).id }
        .sorted()
    val summed = ids.map { it - ids.first() + 1 }.fold(0L) { acc, crt -> acc + crt }
    val total = ids.size * ids.size / 2
    
    for (i in 1 until (ids.size - 1)) {
        if (ids[i] != ids[i - 1] + 1)
            println("Here found ${ids[i - 1]} - ${ids[i]} - ${ids[i + 1]}. Answer is ${ids[i - 1] + 1}")
            
    }
}

/*
 * I don't need the id to compare strings.
 * I know that the ones that go first to the end are bigger than the ones going to the start.
 * The representation is positional
 */
fun compareSeats(seat1: String, seat2: String): Int {
    for (i in (0 .. seat1.length)) {
        if (seat1[i] == seat2[i])
            continue
        if ((seat1[i] == 'F' && seat2[i] == 'B') || (seat1[i] == 'L' && seat2[i] == 'R'))
            return -1
        return 1
    }
    return 0
}

data class Seat(val row: Int, val col: Int, val id: Int) {
    companion object {
        fun compute(desc: String): Seat {
            val it = desc.iterator()
            val row = computeRow(it)
            val col = computeCol(it)
            return Seat(row, col, row * 8 + col)
        }
    }
}



fun computeRow(it: CharIterator) = compute(it, 'F', 0, 127)

fun computeCol(it: CharIterator) = compute(it, 'L', 0, 7)

fun compute(it: CharIterator, firstCmd: Char, start: Int, stop: Int): Int {
    var left = start
    var right = stop
    while (left < right) {
        val cmd = it.next()
        val mid = left + (right - left) / 2
        if (cmd == firstCmd)
            right = mid
        else
            left = mid + 1
    }
    return right
}
