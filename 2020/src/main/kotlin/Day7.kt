fun main() {
    part2()
}

private fun part1() {
    DataReader
        .read(7)
        .asSequence()
        .forEach(GraphBuilder::parseLine)
    
    val node = GraphBuilder.find("shiny gold")!!
    println(GraphBuilder.countReverse(node))
}

private fun part2() {
    DataReader.read(7).asSequence().forEach(GraphBuilder::parseLine)
    
    val node = GraphBuilder.find("shiny gold")!!
    println(GraphBuilder.countInner(node))
}

object GraphBuilder {
    private val headRegex = "^([a-z ]*) bags contain ".toRegex()
    private val tailRegex = " ?(?:((\\d*) ([a-z ]*)) bags?[,\\.])|(no other bag.)".toRegex()
    
    private val allNodes = mutableSetOf<Node>()
    
    fun parseLine(line: String) {
        val head = headRegex.find(line)!!
        val headNode = findOrCreate(head.groups[1]!!.value)
        val edges = tailRegex.findAll(line, head.range.last.plus(1)).flatMap {
            it.destructured.toList()
            val (_, count, color, end) = it.destructured
            if (end.isEmpty()) {
                val newNode = findOrCreate(color)
                newNode.reverseLinks.add(headNode)
                sequenceOf(Edge(newNode, count.toInt()))
            } else {
                emptySequence()
            }
        }
        
        headNode.links.addAll(edges)
    }
    
    private fun findOrCreate(color: String): Node {
        val findOpt = allNodes.find { it.color == color }
        if (findOpt == null) {
            val node = Node(color)
            allNodes.add(node)
            return node
        }
        return findOpt
    }
    
    fun find(color: String) = allNodes.find { it.color == color }
    
    private fun buildAncestry(current: Node): Set<Node> {
        if (current.reverseLinks.isEmpty()) return emptySet()
        return current.reverseLinks + current.reverseLinks.map { buildAncestry(it) }.fold(emptySet()) { acc, it -> acc + it }
    }
    
    fun countInner(current: Node): Int {
        if (current.links.isEmpty()) return 0
        return current.links.map { it.count * countInner(it.other) + it.count }.sum()
    }
    
    fun countReverse(node: Node): Int {
        val a = buildAncestry(node)
        return a.size
    }
}

data class Node(val color: String) {
    val links = mutableListOf<Edge>()
    val reverseLinks = mutableSetOf<Node>()
}

data class Edge(val other: Node, val count: Int)
