fun main() {
    Day13.part2()
}

object Day13 {
    fun part1() {
        val (estimate, schedule) = DataReader
            .read(13)
            .let {
                it[0].toInt() to it[1].split(',')
                    .asSequence().filter { t -> t != "x" }.map { t -> t.toInt() }
            }
        
        schedule
            .map { (it - estimate % it).run { if (this == 0) 0 else this } to it }
            .sortedBy { it.first }
            .first()
            .also {
                println("Answer is ${it.first * it.second}")
            }
    }
    
    /**
     * Part 2
     */
    fun part2() {
        val data = DataReader.read(13)
        val expressions = buildModuloExpressions(data[1].split(","))
        val result = sieve(expressions)
        println("Result is $result")
    }
    
    /**
     * Given an array of busses as described by the problem, it an array of pairs
     *  - first: right hand side of the congruent, a[i]
     *  - second: modulo value, n[i]
     */
    private fun buildModuloExpressions(busses: List<String>): Sequence<Pair<Int, Int>> {
        return busses
            .asSequence()
            .withIndex()
            .filter { it.value != "x" }
            .map { it.index to it.value.toInt() }
            .also { println(it.toList()) }
            .map { (if(it.first == 0) 0 else ((it.second - it.first) % it.second).run {
                /** Sometimes this answer is negative and the default implementation for reminder for negative numbers is not correct for this purpose.
                 *  Plus, we needed to make sure that it.second - it.first is indeed smaller than it.second in absolute value. It was having issues for negative numbers
                 *  **/
                if (this > 0) this else it.second + this
            }) to it.second }
            
    }
    
    fun sieve(expressions: Sequence<Pair<Int, Int>>): Long {
        val sortedExpressions = expressions.sortedByDescending { it.n() }
        println(sortedExpressions.toList())
        fun reduceExpression(crtExp: Pair<Int, Int>, previousX: Long, previousIncrement: Long): Pair<Long, Long> {
            println("Reducing expression $crtExp")
            val result = generateSequence(0) { it + 1 }
                .map { previousX + it * previousIncrement }
                .find { it % crtExp.n() == crtExp.a().toLong() }
            return result!! to previousIncrement * crtExp.n()
        }
        
        val result = sortedExpressions.drop(1)
            .fold(sortedExpressions.first().let { it.a().toLong() to it.n().toLong() }) { acc, crt ->
                reduceExpression(crt, acc.first, acc.second)
            }
        
        return result.first
    }
    
    private fun Pair<Int, Int>.a() = this.first
    private fun Pair<Int, Int>.n() = this.second
}