import java.lang.Exception
import java.util.*
import kotlin.math.sqrt

fun main() {
    Day20.part2()
//    println("123456".toList().subList(1, 5))
}

object Day20 {
    
    fun part1() {
        val tiles = read()

        val map = buildPuzzle(tiles)
        
        val t = map.run {
            listOf(
                first().first().id,
                first().last().id,
                last().first().id,
                last().last().id,
            ).fold(1L) { a, c -> a * c }
        }
        
        println(t)
    }
    
    fun part2() {
        val tiles = read()
        val map = buildPuzzle(tiles)
        val image = map.buildImage()
        
        val dragonCount = image.transformations()
            .map { findDragons(it) }
            .find { it != 0 }

        println(dragonCount)

        val dragonHashes = dragon
            .asSequence()
            .flatMap { it.toList() }
            .count { it == '#' }

        val imageHashes = image
            .asSequence()
            .flatMap { it.toList() }
            .count { it == '#' }

        val result = dragonCount?.let { imageHashes - it * dragonHashes } ?: -1

        println("Hashes are $result")

//        val ii = image.flipVertical().rotate90().rotate90().rotate90()

//        println(image)

//        image.transformations().forEach {
//            println(it.joinToString("\n"))
//            println(findDragons(it))
//            println()
//        }
    }
    
    fun findDragons(image: List<String>): Int {
        var count = 0
        for (l in image.indices) {
            for (c in image[l].indices) {
                if (isDragon(image, l, c))
                    count += 1
            }
        }
        return count
    }
    
    val dragon = listOf(
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
    )
    
    fun isDragon(image: List<String>, line: Int, col: Int): Boolean {
        for (l in dragon.indices) {
            if (line + l >= image.size)
                return false
            for (c in dragon[l].indices) {
                if (col + c >= image[line + l].length)
                    return false
                if (dragon[l][c] == '#' && image[line + l][col + c] != '#')
                    return false
            }
        }
        return true
    }
    
    fun Tile.stripMargins() = content
        .subList(1, content.size - 1)
        .map { it.substring(1, it.length - 1) }
    
    fun Array<Array<Tile>>.buildImage(): List<String> {
        return this
            .map { line ->
                line.map { it.stripMargins() }
                    .reduce { acc, crt ->
                        acc.zip(crt).map { "${it.first}${it.second}" }
                    }
            }.flatten()
    }
    
    fun buildPuzzle(tiles: List<Tile>): Array<Array<Tile>> {
        val taken = Array(tiles.size) { false }
        val size = sqrt(tiles.size.toDouble()).toInt()
        val indexes = goInSpiral(size).toList()
        val map = Array(size) { Array<Tile?>(size) { null } }
        fun buildPuzzleRec(currentPosPointer: Int): Boolean {
            if (currentPosPointer >= indexes.size) {
                return true
            }
            
            val (line, col) = indexes[currentPosPointer]
            val top = if (line > 0) map[line - 1][col] else null
            val bottom = if (line < size - 1) map[line + 1][col] else null
            val left = if (col > 0 ) map[line][col - 1] else null
            val right = if (col < size - 1) map[line][col + 1] else null
            
            for (i in tiles.indices) {
                if (!taken[i]) {
                    val fittedTiles = findMatching(tiles[i], left, top, right, bottom)
                    taken[i] = true
                    for (t in fittedTiles) {
                        map[line][col] = t
                        val foundEnd = buildPuzzleRec(currentPosPointer + 1)
                        if (foundEnd) return true
                    }
                    map[line][col] = null
                    taken[i] = false
                }
            }
            
            return false
        }
        
        val b = buildPuzzleRec(0)
        if (!b) {
            throw Exception("Puzzle not finished")
        }
        return Array(map.size) { line ->
            Array(map[line].size) { col -> map[line][col] ?: throw Exception("Puzzle has nulls") }
        }
    }
    
    fun goInSpiral(size: Int): Sequence<Pair<Int, Int>> {
        fun goInSpiralRec(size: Int, first: Int): Sequence<Pair<Int, Int>> {
            if (size == 0) return sequenceOf()
            if (size == 1) return sequenceOf(first to first)
            var line = first
            var col = first
            return sequence {
                for (i in 0 until size - 1) {
                    yield(line to col)
                    col += 1
                }
                for (i in 0 until size - 1) {
                    yield(line to col)
                    line += 1
                }
                for (i in 0 until size - 1) {
                    yield(line to col)
                    col -= 1
                }
                for (i in 0 until size - 1) {
                    yield(line to col)
                    line -= 1
                }
                yieldAll(goInSpiralRec(size - 2, first + 1))
            }
        }
        return goInSpiralRec(size, 0)
    }
    
    fun List<String>.rotate90(s: Boolean = false): List<String> {
        if (s) println("Here - $size")
        return fold(List(size) { "" }) { acc, crt ->
            acc.zip(crt.toList()).map { (s, c) -> "$s$c" }
        }
    }

    fun List<String>.flipHorizontal() = map { it.reversed() }
    
    fun List<String>.flipVertical() = reversed()
    
    fun List<String>.allOrientations() = sequence {
        yield(this@allOrientations)
        val fh = flipHorizontal()
        yield(fh)
        yield(flipVertical())
        yield(fh.flipVertical())
    }
    
    fun List<String>.transformations() = sequence {
        yieldAll(allOrientations())
        yieldAll(rotate90().allOrientations())
    }
    
    data class Tile(val id: Int, var content: List<String>) {
        fun top() = content.first()
        fun bottom() = content.last()
        fun right() = content.map { it.last() }.joinToString("")
        fun left() = content.map { it.first() }.joinToString("")
    
        fun rotate90() =
            content.rotate90().let { Tile(id, it) }
        
        fun flipHorizontal() =
            Tile(id, content.flipHorizontal())
        
        fun flipVertical() =
            Tile(id, content.flipVertical())
        
        private fun allOrientations(t: Tile) = sequence {
            val s = t.content.allOrientations()
                .map { Tile(id, it) }
            yieldAll(s)
        }
        
        fun transformations() = sequence {
            yieldAll(allOrientations(this@Tile))
            val r = rotate90()
            yieldAll(allOrientations(r))
        }
    }
    
    private val matchingFunctions = listOf<(Tile) -> (Tile) -> Boolean>(
        { referenceLeft -> { tileToMatch -> referenceLeft.right() == tileToMatch.left() } },
        { referenceTop -> { tileToMatch -> referenceTop.bottom() == tileToMatch.top() } },
        { referenceRight -> { tileToMatch -> referenceRight.left() == tileToMatch.right() } },
        { referenceBottom -> { tileToMatch -> referenceBottom.top() == tileToMatch.bottom() } },
    )
    
    fun findMatching(tileToMatch: Tile,
                     referenceLeft: Tile? = null,
                     referenceTop: Tile? = null,
                     referenceRight: Tile? = null,
                     referenceBottom: Tile? = null,
    ): Sequence<Tile> {
        val criteria = { c: Tile ->
            listOf(referenceLeft, referenceTop, referenceRight, referenceBottom)
                .zip(matchingFunctions)
                .filter { it.first != null }
                .map { it.second(it.first!!) }
                .fold(true) { acc, crt -> acc && crt(c) }
        }
        
        return tileToMatch.transformations()
            .filter { criteria(it) }
    }
    
    val regex = "Tile (\\d*):".toRegex()
    fun read(): List<Tile> {
        return DataReader.read(20)
            .chunked(12)
            .map {
                val (number) = regex.find(it.first())!!.destructured
                Tile(number.toInt(), it.subList(1, it.size - 1))
            }
    }
}