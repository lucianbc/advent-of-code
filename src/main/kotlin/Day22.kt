import java.lang.NumberFormatException
import java.util.*

fun main() {
    Day22.part2()
}

object Day22 {
    fun part2() {
//        val p1 = listOf(9, 2, 6, 3, 1)
//        val p2 = listOf(5, 8, 4, 7, 10)
        
        val (p1, p2) = read()
        
        val g = Game(p1, p2).play()
        println(g.second.computeScore())
    }
    
    var gameCounter = 1
    
    class Game(val player1: List<Int>, val player2: List<Int>) {
        private val count = gameCounter++
        private var roundCounter = 1
        
        private val previousRounds = mutableListOf<Pair<List<Int>, List<Int>>>()
        
        enum class Winner { P1, P2 }
        
        fun play(): Pair<Winner, List<Int>> {
            return playGameIt(player1, player2)
        }
        
        fun playGame(player1: List<Int>, player2: List<Int>): Pair<Winner, List<Int>> {
            val roundWinner = playRound(player1, player2)
            val nextPlayer1 =
                if (roundWinner == Winner.P1) player1.drop(1).plus(listOf(player1.first(), player2.first()))
                else player1.drop(1)
            val nextPlayer2 =
                if (roundWinner == Winner.P2) player2.drop(1).plus(listOf(player2.first(), player1.first()))
                else player2.drop(1)
    
            return when {
                nextPlayer1.isEmpty() -> Winner.P2 to nextPlayer2
                nextPlayer2.isEmpty() -> Winner.P1 to nextPlayer1
                else -> playGame(nextPlayer1, nextPlayer2)
            }
        }
        
        fun playGameIt(_player1: List<Int>, _player2: List<Int>): Pair<Winner, List<Int>> {
            var player1 = _player1
            var player2 = _player2
            
            while (player1.isNotEmpty() && player2.isNotEmpty()) {
                val roundWinner = playRound(player1, player2)
                val np1 =
                    if (roundWinner == Winner.P1) player1.drop(1).plus(listOf(player1.first(), player2.first()))
                    else player1.drop(1)
                val np2 =
                    if (roundWinner == Winner.P2) player2.drop(1).plus(listOf(player2.first(), player1.first()))
                    else player2.drop(1)
                player1 = np1
                player2 = np2
            }
            return if (player1.isEmpty()) Winner.P2 to player2 else Winner.P1 to player1
        }
        
        fun playRound(player1: List<Int>, player2: List<Int>): Winner {
            println("-- Round ${roundCounter++} (Game ${count}) --")
            println("Player 1's deck: $player1")
            println("Player 2's deck: $player2")
            val f = previousRounds.find { it == player1 to player2 }
            if (f != null)
                return Winner.P1
            previousRounds.add(player1 to player2)
            
            val p1 = player1.first()
            val p2 = player2.first()
    
            return if (player1.size >= p1 + 1 && player2.size >= p2 + 1) {
                // start new game
                val take1 = player1.drop(1).take(p1)
                val take2 = player2.drop(1).take(p2)
                val recursiveGame = Game(take1, take2)
                recursiveGame.play().first
            } else {
                if (p1 > p2) Winner.P1
                else Winner.P2
            }
        }
    }
    
    fun part1() {
//        val (p1, p2) = read()
        val p1 = LinkedList(listOf(8, 49, 26, 40, 25, 48, 24, 35, 31, 45, 16, 38, 4))
        val p2 = LinkedList(listOf(36, 29, 34, 27, 32, 13, 47, 20, 46, 1))
        val w = play(p1, p2)
        println(w.computeScore())
    }
    
    fun play(player1: Queue<Int>, player2: Queue<Int>): Queue<Int> {
        when {
            player1.isEmpty() -> {
                return player2
            }
            player2.isEmpty() -> {
                return player1
            }
            else -> {
                val p1 = player1.poll()
                val p2 = player2.poll()
                if (p1 < p2) {
                    player2.add(p2)
                    player2.add(p1)
                } else if (p1 > p2) {
                    player1.add(p1)
                    player1.add(p2)
                }
                return play(player1, player2)
            }
        }
    }
    
    fun Iterable<Int>.computeScore() = reversed().withIndex().fold(0L) { acc, crt ->
        acc + (crt.index + 1) * crt.value
    }
    
    fun read(): Pair<LinkedList<Int>, LinkedList<Int>> {
        val data = DataReader.read(22)
        
        val player1 = LinkedList<Int>()
        val player2 = LinkedList<Int>()
        
        val iterator = data.iterator()
        
        while (iterator.hasNext()) {
            val crt = iterator.next()
            if (crt == "Player 2:") {
                iterator.forEachRemaining {
                    player2.add(it.toInt())
                }
            } else {
                val number = try { crt.toInt() } catch (e: NumberFormatException) { null }
                if (number != null) {
                    player1.add(number)
                }
            }
        }
        return player1 to player2
    }
}
