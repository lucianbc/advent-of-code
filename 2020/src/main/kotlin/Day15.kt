fun main() {
    Day15.part1()
}

object Day15 {
    val lastSpoken = mutableMapOf<Int, Int>()
    val lastLastSpoken = mutableMapOf<Int, Int>()
    
    fun speak(lastSpokenNumber: Int, currentRound: Int): Int {
        val lastSeen = lastSpoken[lastSpokenNumber]!!
        val lastLastSeen = lastLastSpoken[lastSpokenNumber]
    
        return if (lastLastSeen == null) { // only saw the number once
            lastLastSpoken[0] = lastSpoken[0]!!
            lastSpoken[0] = currentRound
            0
        } else {
            val newSpoken = lastSeen - lastLastSeen
            lastSpoken[newSpoken]?.let {
                lastLastSpoken[newSpoken] = it
            }
            lastSpoken[newSpoken] = currentRound
            newSpoken
        }
    }
    
    fun part1() {
        val numbers = listOf(1,20,11,6,12,0)
        numbers.withIndex().forEach {
            lastSpoken[it.value] = it.index + 1
        }
        
        val upTo = 30000000
        var last = numbers.last()
        
        for (i in (numbers.size + 1)..upTo) {
            print("\r$i / $upTo")
            last = speak(last, i)
            
        }
        
        println()
        println(last)
    }
}