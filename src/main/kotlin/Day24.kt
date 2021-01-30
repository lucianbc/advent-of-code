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

//        performDirections(parseLine("nwwsw")).also { println(it) }

//        read()
//            .take(8)
//            .forEach { Swarm.identifyNew(it) }

        sequenceOf(
            sequenceOf(Direction.East, Direction.East, Direction.East),
            sequenceOf(Direction.SouthEast, Direction.SouthEast, Direction.East, Direction.NorthEast, Direction.NorthEast),
//            sequenceOf(Direction.SouthWest),
//            sequenceOf(Direction.West),
//            sequenceOf(Direction.NorthWest),
//            sequenceOf(Direction.NorthEast),
        ).forEach { Swarm.identifyNew(it) }
        
        println(Swarm.countAll())
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
    
    object Swarm {
        var tileId = 0
        class Tile(
            var turns: Int = 0,
            var e: Tile? = null,
            var se: Tile? = null,
            var sw: Tile? = null,
            var w: Tile? = null,
            var nw: Tile? = null,
            var ne: Tile? = null,
        ) {
            val it = tileId++
            /*
            Prime example of where 'pass-by-reference' would help. If I had a way to pass the pointer by reference,
            I could have modified it generically, saving me 5 lines basically
             */
            fun goTo(direction: Direction): Tile = when (direction) {
                Direction.East -> e ?: Tile().also {
                    e = it; it.w = this
                    it.sw = se; se?.ne = it
                    it.nw = ne; ne?.se = it
                }
                Direction.SouthEast -> se ?: Tile().also {
                    se = it; it.nw = this
                    it.ne = e; e?.sw = it
                    it.w = sw; sw?.e = it
                }
                Direction.SouthWest -> sw ?: Tile().also {
                    sw = it; it.ne = this
                    it.e = se; se?.w = it
                    it.nw = w; w?.se = it
                }
                Direction.West -> w ?: Tile().also {
                    w = it; it.e = this
                    it.se = sw; sw?.nw = it
                    it.ne = nw; nw?.sw = it
                }
                Direction.NorthWest -> nw ?: Tile().also {
                    nw = it; it.se = this
                    it.e = ne; ne?.w = it
                    it.sw = w; w?.ne = it
                }
                Direction.NorthEast -> ne ?: Tile().also {
                    ne = it; it.sw = this
                    it.se = e; e?.nw = it
                    it.w = nw; nw?.e = it
                }
            }
        }
        
        private val destinations = HashSet<Tile>()
        
        private val reference = Tile()
        
        fun identifyNew(directions: Sequence<Direction>) {
            directions.fold(reference) { acc, crt -> acc.goTo(crt) }
                .also { it.turns += 1; destinations.add(it) }
        }
        
        fun countAll(): Map<Int, Int> {
            return destinations
                .groupBy { it.turns }
                .mapValues { it.value.size }
        }
    }
}
