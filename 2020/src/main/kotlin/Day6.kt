fun main() {
    println(Solution.part2())
}

private object Solution {
    fun part1() = DataReader.read(6)
        .fold(sequenceOf(emptySet<Char>())) { acc, crt ->
            if (crt.isEmpty()) {
                return@fold sequenceOf(emptySet<Char>()) + acc
            }
            val h = acc.first() + crt.toList()
            val t = acc.drop(1)
            sequenceOf(h) + t
        }
        .map { it.size }
        .sum()
    
    
    fun part2(): Int {
        val data = DataReader.read(6)
        val iterator = data.iterator()
        
        var current = iterator.next()
        var currentSet = current.toSet()
        
        return sequence {
            while (iterator.hasNext()) {
                current = iterator.next()
                currentSet = if (current.isEmpty()) {
                    yield(currentSet)
                    if (iterator.hasNext()) {
                        iterator.next().toSet()
                    } else {
                        emptySet()
                    }
                } else {
                    currentSet.intersect(current.toSet())
                }
            }
            if (currentSet.isNotEmpty()) {
                yield(currentSet)
            }
        }.map { it.size }.sum()
    }
}
