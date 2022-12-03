fun main() {
    part2()
}

private fun part1() {
    val numbers = DataReader.read(9).map { it.toLong() }.toLongArray()
    CypherOps(25, numbers).validateCypher()
}

private fun part2() {
    val numbers = DataReader.read(9).map { it.toLong() }.toLongArray()
    val target = 25918798L
    val window = CypherOps(25, numbers).findSubarrayForSum(target)
    if (window == null) {
        println("Failed to find window")
    } else {
        println("Window is $window. Sum is ${numbers[window.first] + numbers[window.second - 1]}")
        val s = (window.first until window.second).map { numbers[it] }.let {
            it.min()!! to it.max()!!
        }
        println("Min and max are $s and they sum to ${s.first + s.second}")
    }
}

class CypherOps(private val preambleLength: Int, private val numbers: LongArray) {
    private fun checkNumberAt(position: Int): Long? {
        val numberToCheck = numbers[position]
        val startIndex = position - preambleLength
        
        return (0 until preambleLength - 1)
            .flatMap { (it + 1 until preambleLength).map { it2 -> startIndex + it to  startIndex + it2 } }
            .map { numbers[it.first] + numbers[it.second] }
            .find { it == numberToCheck }
    }

    fun validateCypher() {
        for (i in (25 until numbers.size)) {
            val f = checkNumberAt(i)
            if (f == null) {
                println("Number ${numbers[i]} doesn't match")
                break
            }
        }
    }
    
    fun findSubarrayForSum(sum: Long): Pair<Int, Int>? {
        var startIncl = 0
        var endExcl = 1
        var currentSum = numbers[0]
        while (endExcl <= numbers.size) {
            if (currentSum == sum)
                return startIncl to endExcl
            if (currentSum < sum) {
                currentSum += numbers[endExcl]
                endExcl++
            } else {
                currentSum -= numbers[startIncl]
                startIncl++
            }
        }
        return null
    }
}
