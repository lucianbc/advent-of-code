import java.lang.Exception

fun main() {
    Day24.part2()
}

object Day24 {
    fun part1() {
        val r = read()
            .map { performDirections(it) }
            .groupBy { it }
            .mapValues { it.value.size }
            .asSequence()
            .groupBy { it.value }
            .mapValues { it.value.size }

        // expected: 455
        println(r.filter { it.key % 2 == 1 }.values.sum())
    }
    
    fun part2() {
        val arrangement = initialArrangement(initialBlacks(read()))
        val final = (0..99).fold(arrangement) { acc, crt ->
            println("Day $crt: ${acc.size}")
            nextArrangement(acc)
        }
        
        // expected: 3904
        println("Day 100: ${final.size}")
    }
    
    fun performDirections(directions: Sequence<Direction>) =
        directions.fold(Tile(0, 0)) { acc, crt -> acc.goTo(crt) }
    
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
    
    /**
     * axis 1 is left/right
     * axis 2 is top/bottom
     */
    data class Tile(val axis1: Int, val axis2: Int) {
        fun goTo(direction: Direction) = when (direction) {
                Direction.East -> Tile(axis1 + 2, axis2)
                Direction.SouthEast -> Tile(axis1 + 1, axis2 - 1)
                Direction.SouthWest -> Tile(axis1 - 1, axis2 - 1)
                Direction.West -> Tile(axis1 - 2, axis2)
                Direction.NorthWest -> Tile(axis1 - 1, axis2 + 1)
                Direction.NorthEast -> Tile(axis1 + 1, axis2 + 1)
            }
        
        fun neighbors() = sequenceOf(
            Tile(axis1 + 2, axis2),
            Tile(axis1 + 1, axis2 - 1),
            Tile(axis1 - 1, axis2 - 1),
            Tile(axis1 - 2, axis2),
            Tile(axis1 - 1, axis2 + 1),
            Tile(axis1 + 1, axis2 + 1),
        )
    }
    
    data class Arrangement(val blackTiles: Set<Tile>, val top: Int, val bottom: Int, val left: Int, val right: Int)
        : Set<Tile> by blackTiles
    
    fun initialBlacks(directions: Sequence<Sequence<Direction>>) =
        directions
            .map { performDirections(it) }
            .groupBy { it }
            .filter { it.value.size % 2 == 1 }
            .keys
    
    fun initialArrangement(blackTiles: Set<Tile>): Arrangement {
        val (top, bottom, left, right) = findBounds(blackTiles)
        return Arrangement(blackTiles, top, bottom, left, right)
    }
    
    fun nextArrangement(currentArrangement: Arrangement): Arrangement {
        val nextBlackTiles = (currentArrangement.top downTo currentArrangement.bottom)
            .asSequence()
            .flatMap { a2 ->
                (currentArrangement.left .. currentArrangement.right).asSequence().map { a1 -> a2 to a1 }
            }
            .map { (a2, a1) -> Tile(a1, a2) }
            .filter { tile ->
                val isBlack = currentArrangement.contains(tile)
                val blackNeighbors = tile.neighbors().count { currentArrangement.contains(it) }
                if (isBlack) !(blackNeighbors == 0 || blackNeighbors > 2)
                else blackNeighbors == 2
            }.toHashSet()
            
        return findBounds(nextBlackTiles).run {
            Arrangement(nextBlackTiles, top, bottom, left, right)
        }
    }
    
    fun findBounds(blackTiles: Set<Tile>) =
        blackTiles.fold(Coords(0, 0,0, 0)) { (top, bottom, left, right), crt ->
            Coords(
                if (crt.axis2 > top) crt.axis2 else top,
                if (crt.axis2 < bottom) crt.axis2 else bottom,
                if (crt.axis1 < left) crt.axis1 else left,
                if (crt.axis1 > right) crt.axis1 else right,
            )
        }.run { Coords(top + 1, bottom - 1, left - 2, right + 2) }
    
    data class Coords(val top: Int, val bottom: Int, val left: Int, val right: Int)
}
