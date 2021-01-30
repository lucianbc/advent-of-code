import java.lang.Exception

fun main() {
    Day24.part1()
}

object Day24 {
    
    fun part1() {
//        val r = read()
//            .map { performDirections(it) }
//            .groupBy { it }
//            .asSequence()
//            .groupBy { it.value.count() }
//            .mapValues { it.value.count() }

//        println(r)
        
        performDirections(parseLine("nwwsw")).also { println(it) }
    }
    
    fun performDirections(directions: Sequence<Direction>) =
        directions.fold(Tile(0, 0, 0)) { acc, crt -> acc.goTo(crt) }
    
    fun read() = DataReader.read(24)
        .asSequence()
        .map { parseLine(it) }
    
    val regex = "(e)|(se)|(sw)|(w)|(nw)|(ne)".toRegex()
    fun parseLine(line: String): Sequence<Direction> {
        return regex.findAll(line)
            .map { when (it.value) {
                "e" -> Direction.East
                "se" -> Direction.SouthEast
                "sw" -> Direction.SouthWest
                "w" -> Direction.West
                "nw" -> Direction.NorthWest
                "ne" -> Direction.NorthEast
                else -> throw Exception("Parse error - direction for ${it.value} not found")
            } }
    }
    
    sealed class Direction {
        object East : Direction()
        object SouthEast : Direction()
        object SouthWest : Direction()
        object West : Direction()
        object NorthWest : Direction()
        object NorthEast : Direction()
    }
    
    data class Tile(val axis1: Int, val axis2: Int, val axis3: Int) {
        fun goTo(direction: Direction) = when (direction) {
                Direction.East -> Tile(axis1 + 1, axis2, axis3)
                Direction.SouthEast -> Tile(axis1, axis2 + 1, axis3)
                Direction.SouthWest -> Tile(axis1, axis2, axis3 + 1)
                Direction.West -> Tile(axis1 - 1, axis2, axis3)
                Direction.NorthWest -> Tile(axis1, axis2 - 1, axis3)
                Direction.NorthEast -> Tile(axis1, axis2, axis3 - 1)
            }
    }
}
