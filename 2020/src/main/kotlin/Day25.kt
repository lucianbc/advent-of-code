fun main() {
    Day25.part1()
}

object Day25 {
    fun part1() {
       encryptionFlow(11404017, 13768789)
//        encryptionFlow(5764801, 17807724)
    }
    
    fun encryptionFlow(cardPK: Long, doorPK: Long) {
        val subjectNumber = 7L
        println("Finding card loop")
        val cardLoop = findLoop(subjectNumber, cardPK)
//            generateSequence(1L) { it + 1 }
//            .find { loop ->
//                print("Trying $loop\r")
//                transform(loop)(subjectNumber) == cardPK
//            }!!
        
        println("Card loop - $cardLoop")
        println("Finding door loop")
        val doorLoop =
            findLoop(subjectNumber, doorPK)
//            generateSequence(1L) { it + 1}
//            .find { loop -> transform(loop)(subjectNumber) == doorPK }!!
        println("Door loop - $doorLoop")
        
        val ek1 = transform(cardLoop)(doorPK)
        val ek2 = transform(doorLoop)(cardPK)
        
        println("$ek1 - $ek2")
    }
    
    val transform: (Long) -> (Long) -> Long = { loopSize -> { subjectNumber ->
        var start = 1L
        for (i in (1..loopSize)) {
            start = (start * subjectNumber) % 20201227L
        }
        start
    } }
    
    fun findLoop(seed: Long, targetTransform: Long): Long {
        var currentTransform = 1L
        var currentLoop = 0L
        while (currentTransform != targetTransform) {
            currentLoop += 1
            currentTransform = (currentTransform * seed) % 20201227L
        }
        return currentLoop
    }
}