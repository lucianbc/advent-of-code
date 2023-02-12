import java.util.TreeSet
import kotlin.math.max

fun main() {
  Day14.part2()
}

object Day14 {
  fun part1() {
    val input = DataReader.read(14)
    val map = buildMap(input)

    println(map.print('0'))
    println()

    var counter = 0
    do {
      val x = map.drop(500, 0)
      counter++
      if (x != null) {
        map.add(x.first, x.second)
      }
    } while (x != null)

    println(map.print('0'))

    println(counter - 1)
  }

  fun part2() {
    val input = DataReader.read(14)
    val map = buildMap(input)

    val floorY = map.cols.flatMap { it.value }
      .max() + 2

    val xLeft = max(map.cols.keys.min() - floorY, 0)
    val xRight = map.cols.keys.max() + floorY

    (xLeft .. xRight).forEach {
      map.add(it, floorY)
    }

    var counter = 0
    do {
      val x = map.drop(500, 0)
      counter++
      if (x != null) {
        map.add(x.first, x.second)
      }
    } while (x?.second != 0)

    println(map.print('0'))

    println(counter)
  }

  fun iterate(from: Int, to: Int): Sequence<Int> {
    return if (from >= to) {
      (from downTo to)
    } else {
      (from .. to)
    }.asSequence()
  }

  fun buildMap(lines: List<String>): Map {
    val map = Map()

    lines.asSequence()
      .flatMap { line ->
        line.split(" -> ")
          .map { it.split(',').run { this[0].toInt() to this[1].toInt() } }
          .zipWithNext()
          .flatMap {
            val (from, to) = it
//            println("From $from to $to")
            if (from.first == to.first) {
              iterate(from.second, to.second).map { from.first to it }
            } else {
              iterate(from.first, to.first).map { it to from.second }
            }
          }
      }.forEach {
//        println("Adding ${it}")
        map.add(it.first, it.second)
      }

    return map
  }

  class Map {
    val cols: HashMap<Int, TreeSet<Int>> = HashMap()

    fun print(marker: Char): String {
      val left = cols.keys.min()
      val right = cols.keys.max()
      val size = right - left + 1
      val arr = Array<CharArray>(size) { line ->
        CharArray(size) { col ->
          val mapX = col + left
          val mapY = line
//          println("Checking $mapX $mapY with $col $line")
          if (cols[mapX]?.contains(mapY) == true) {
            marker
          } else {
            '.'
          }
        }
      }
      return arr.joinToString("\n") {
        it.joinToString("")
      }
    }

    fun add(x: Int, y: Int) {
      if (cols.containsKey(x)) {
        cols[x]?.add(y)
      } else {
        cols[x] = TreeSet<Int>().apply { add(y) }
      }
    }

    // returns the first spot available below a specific value
    fun findFirstBelow(x: Int, y: Int): Pair<Int, Int>? {
      if (cols.containsKey(x)) {
        val column = cols[x]!!
        val row = column.ceiling(y)
        if (row != null) {
          return x to (row - 1)
        }
      }
      return null
    }

    fun isCeilAvailable(x: Int, y: Int): Boolean {
      return cols[x]?.contains(y) != true;
    }

    fun drop(x: Int, y: Int): Pair<Int, Int>? {
      val p1 = findFirstBelow(x, y)
      if (p1 != null) {
        val left = p1.first - 1 to p1.second + 1
        val right = p1.first + 1 to p1.second + 1
        return if (isCeilAvailable(left.first, left.second)) {
          drop(left.first, left.second)
        } else if (isCeilAvailable(right.first, right.second)) {
          drop(right.first, right.second)
        } else {
          p1
        }
      }
      return null
    }
  }
}
