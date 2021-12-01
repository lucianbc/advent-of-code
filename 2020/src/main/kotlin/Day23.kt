import java.lang.Exception

fun main() {
    Day23.part2()
}

object Day23 {
    fun part1() {
        val input = intArrayOf(4,9,6,1,3,8,5,2,7)
        
        val c = CrabMove(input)
        val p = 0
        
        val result = (1..100).fold(input to p) { acc, i ->
            println("$i - ${acc.first.toList()}")
            c.nextMove(acc.first, acc.second)
        }
        
        val indexOf1 = result.first.indexOf(1)
        
        (1 until result.first.size)
            .map { result.first[(indexOf1 + it) % result.first.size] }
            .joinToString("")
            .also { println("Result is $it") }
        
    }
    
    fun part2() {
        println("Starting program")
        
        val maxValue = 1000000
        val numOfRounds = 10000000
        
        val input = parseInput("496138527")
        
        val s = input.asSequence()
            .plus((10..maxValue).asSequence())
        
        println(s.take(20).joinToString(","))
        
        val circle = createCircularList(s)
        
        println("Finished building list. Starting rounds")
        
        val next = (1..numOfRounds).fold(circle) { crt, i ->
//            println("Round $i")
//            println(crt.wrap(9))
            crabMove(crt)
        }
        
        println(next.wrap(9))
        
        println("Ended rounds")
    
        val node1 = next.findNode(1)
        
        val n1 = node1.next.value
        val n2 = node1.next.next.value
        
        println("$n1 - $n2 - ${n1.toLong() * n2.toLong()}")
    }
    
    fun parseInput(value: String) = value.toList().map { "$it".toInt() }
    
    class CrabMove(initialState: IntArray) {
        val max = initialState.maxOrNull()!!
        val min = initialState.minOrNull()!!
        
        fun nextMove(currentState: IntArray, currentPos: Int): Pair<IntArray, Int> {
            val nextElems = sequenceOf(currentPos + 1, currentPos + 2, currentPos + 3)
                .map { currentState[it % currentState.size] }
                .toSet()
        
            val destinationCup = generateSequence(1) { it + 1 }
                .map { wrappedToRight(currentState[currentPos] - it) }
                .find {
                    !nextElems.contains(it)
                }!!
            
            val destinationPos = currentState.indexOf(destinationCup)
            
            return doMoves(currentState, currentPos, destinationPos) to (currentPos + 1).rem2(currentState.size)
        }
        
        fun wrappedToRight(value: Int) = if (value < min) max + (value - min + 1) else value
    }
    
    fun doMoves(currentState: IntArray, currentPos: Int, destinationPos: Int): IntArray {
        return currentState.copyOf().apply {
            var i = 1
            while ((currentPos + i).rem2(size) != destinationPos) {
                this[(currentPos + i).rem2(size)] = this[(currentPos + i + 3).rem2(size)]
                i++
            }
            
            (-2..0).forEach {
                this[(destinationPos + it).rem2(size)] = currentState[(currentPos + 3 + it).rem2(size)]
            }
        }
    }
    
    fun Int.rem2(other: Int): Int {
        return if (this < 0) other + this else this % other
    }
    
    fun Node.wrap(len: Int) = (1..len).fold("" to this) { (str, crt), _ -> "${str}${crt.value}" to crt.next }
    
    fun Node.wrapBack(len: Int) = (1..len).fold("" to this) { (str, crt), _ -> "${str}${crt.value}" to crt.previous }
    
    // Part 2
    class Node(
        val value: Int,
    ) {
        init {
            allNodes[value] = this
            if (value < min) min = value
            if (value > max) max = value
        }
        
        private var _next: Node? = null
        private var _previous: Node? = null
        
        var next: Node
            get() = _next!!
            set(value) { _next = value }
        
        var previous: Node
            get() = _previous!!
            set(value) { _previous = value }
        
        fun findNode(value: Int): Node {
            return allNodes[value]!!
//            return if (this.value == value) this
//            else this.next.findNode(value)
        }
        
        companion object {
            val allNodes = mutableMapOf<Int, Node>()
            var min = 1
            var max = 9
        }
    }
    
    fun createCircularList(elements: Sequence<Int>): Node {
        val initial: Pair<Node, Node>? = null
        
        val (first, last) = elements.fold(initial) { acc, crt ->
            when (acc) {
                null -> Node(crt).let { it to it }
                else -> {
                    val (first, current) = acc
                    val n = Node(crt)
                    current.next = n
                    n.previous = current
                    first to n
                }
            }
        }?: throw Exception("Empty list passed")
        
        last.next = first
        first.previous = last
        return first
    }
    
    fun crabMove(current: Node): Node {
        val (next3, displaceStart) = sequenceOf(1, 2, 3)
            .fold(emptySet<Int>() to current.next) { (set, crt), _ -> set.plus(crt.value) to crt.next }
        
        val destinationVal = boundedSequence(current.value - 1, Node.min, Node.max)
            .find { !next3.contains(it) }!!
        
        val destinationNode = current.findNode(destinationVal)
        
        val strippedStart = current.next
        val strippedEnd = displaceStart.previous
        
        current.next = strippedEnd.next
        strippedEnd.next.previous = current
    
        strippedEnd.next = destinationNode.next
        destinationNode.next.previous = strippedEnd
        
        destinationNode.next = strippedStart
        strippedStart.previous = destinationNode
        
        return current.next
    }
    
    fun boundedSequence(start: Int, min: Int, max: Int) = sequence {
        var crt = if (start < min) max else start
        while (true) {
            yield(crt)
            crt = if (crt == min) max else crt - 1
        }
    }
}