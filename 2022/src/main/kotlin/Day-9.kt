import kotlin.math.abs

fun main() {
  Day9.part2()
}

object Day9 {
  data class Position(val x: Int, val y: Int) {
    fun move(direction: Char): Position {
      return when (direction) {
        'L' -> copy(x = x - 1, y = y)
        'R' -> copy(x = x + 1, y = y)
        'U' -> copy(x = x, y = y - 1)
        'D' -> copy(x = x, y = y + 1)
        else -> throw IllegalArgumentException("Bad direction $direction")
      }
    }

    fun isAdjacentTo(other: Position): Boolean {
      if (abs(x - other.x) <= 1 && abs(y - other.y) <= 1)
        return true
      return false
    }
  }

  class Rope(length: Int) {
    private var knots = Array(length) { _ -> Position(0, 0) }
    private var last = length - 1
    private val tailHistory = mutableSetOf(Position(0, 0))

    private val left = -40
    private val right = 40
    private val top = -40
    private val bottom = 40
    private fun mapPosition(p: Position): Pair<Int, Int> {
      return p.y - top to p.x - left
    }


     /*

     H: (2, -2), T1: (1, 0) => (2, -1)

     x+= (h.x - t.x) / abs(h.x - t.x)


     h: -1, t: 1  (h.y - t.y) / abs(h.y - t.y)

     h: 1, t: -1


      */
    private fun move(direction: Char) {
      val newKnots = knots.clone()
      newKnots[last] = newKnots[last].move(direction)
      (last - 1 downTo 0).forEach { crtNode ->
        val nextNode = crtNode + 1
        val head = newKnots[nextNode]
        val tail = newKnots[crtNode]
        if (!tail.isAdjacentTo(head)) {
          newKnots[crtNode] = Position(
            x = tail.x + if (head.x - tail.x == 0) 0 else (head.x - tail.x) / abs(head.x - tail.x),
            y = tail.y + if (head.y - tail.y == 0) 0 else (head.y - tail.y) / abs(head.y - tail.y),
          )
        }
      }
      tailHistory.add(newKnots[0])
      knots = newKnots
//      renderRope().let { println(it) }
//      System.`in`.read()
    }

    fun execute(command: Command) {

      with(command) {
        (1..steps).forEach {
          move(direction)
        }
      }
    }

    fun getTailHistoryLength(): Int {
//      println(tailHistory)
      return tailHistory.size
    }

    private fun generateCanvas(): Array<Array<Char>> {
      val width = right - left + 1
      val height = bottom - top + 1

      return Array(height) { _ -> Array(width) { _ -> '.'} }
    }

    fun renderRope(): String {
      val x = generateCanvas()

      for (knotIndex in knots.indices.reversed()) {
        val p = mapPosition(knots[knotIndex])
        if (x[p.first][p.second] == '.') {
          val knotName = if (knotIndex == knots.size - 1) 'H' else (9 - knotIndex).digitToChar()
          x[p.first][p.second] = knotName
        }
      }
      val s = mapPosition(Position(0, 0))
      if (x[s.first][s.second] == '.') {
        x[s.first][s.second] = 's'
      }

      return x.joinToString("\n") { it.joinToString("") } + "\n"
    }

    fun printTail() {
//      val minX = tailHistory.map { it.x }.min()
//      val maxX = tailHistory.map { it.x }.max()
//      val minY = tailHistory.map { it.y }.min()
//      val maxY = tailHistory.map { it.y }.max()

      val minX = -11
      val maxX = 14
      val minY = -15
      val maxY = 5

      // _ 1 2 3 4
      val height = maxY - minY + 1
      val width = maxX - minX + 1

      val map = Array(height) { _ ->
        Array(width) { _ -> '.'}
      }

      tailHistory.forEach { (x, y) ->
        map[y - minY][x - minX] = '#'
      }

      map[-minX][-minY] = 's'

      val x = map.map { it.joinToString("") }

      x.forEach { println(it) }
    }
  }

  data class Command(val direction: Char, val steps: Int) {
    fun execute(head: Position, tail: Position): Triple<Position, Position, MutableSet<Position>> {
      var crtHead = head
      var crtTail = tail
      val tailHistory = mutableSetOf(tail)
      (1..steps).forEach {
        val newHead = crtHead.move(direction)
        if (!newHead.isAdjacentTo(crtTail)) {
          val newTail = crtHead
          tailHistory.add(newTail)
          crtTail = newTail
        }
        crtHead = newHead
      }
      return Triple(crtHead, crtTail, tailHistory)
    }
  }

  fun part1() {
    val input = DataReader.read(9)
      .map {
        val s = it.split(" ")
        Command(s[0].first(), s[1].toInt())
      }

    val head = Position(0, 0)
    val tail = Position(0, 0)
    val tailHistory = setOf(tail)

    val (_, _, hist) = input.fold(Triple(head, tail, tailHistory)) { (head, tail, history), crt ->
      val (newHead, newTail, newHist) = crt.execute(head, tail)
      println("Command is $crt")
      println("Head: $newHead, Tail: $newTail")
      println("Hist: $newHist")
      println("=====================\n\n")

      Triple(newHead, newTail, history + newHist)
    }

    println("Result is ${hist.size}")
  }

  fun part2() {
    val input = DataReader.read(9)
      .map {
        val s = it.split(" ")
        Command(s[0].first(), s[1].toInt())
      }

    val rope = Rope(10)
    input.forEach { rope.execute(it) }

    println("Result is ${rope.getTailHistoryLength()}")
  }
}
