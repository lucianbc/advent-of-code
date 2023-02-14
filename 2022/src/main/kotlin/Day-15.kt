import kotlin.math.abs

fun main() {
  Day15.part1()
}

object Day15 {
  data class SensorReading(val sx: Int, val sy: Int, val bx: Int, val by: Int) {
    val radius = abs(sx - bx) + abs(sy - by)

    val top = Point(sx, sy - radius)
    val bottom = Point(sx, sy + radius)
    val left = Point(sx - radius, sy)
    val right = Point(sx + radius, sy)

    fun intersectionSegment(y: Int): Pair<Int, Int>? {
      val distToY = abs(sy - y)

      if (distToY > radius) {
        return null
      }

      val stepsLeft = radius - distToY

      val left = sx - stepsLeft
      val right = sx + stepsLeft

      return left to right
    }
  }

  fun parseInput(): List<SensorReading> {
    val input = DataReader.read(15)
    val readings = input.map { line ->
      "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)".toRegex()
        .matchEntire(line)
        .let {
          val groups = it!!.groups
          SensorReading(
            groups[1]!!.value.toInt(),
            groups[2]!!.value.toInt(),
            groups[3]!!.value.toInt(),
            groups[4]!!.value.toInt(),
          )
        }
    }
    return readings
  }
  fun part1() {
//    6078701
    val readings = parseInput()

    val refY = 2000000

    val points = readings.mapNotNull {
      val inter = it.intersectionSegment(refY)
      if (inter != null) {
//        val (l, r) = inter
//        if (it.by == refY) {
//          println("${it.bx}, $l, $r")
//        }
//        if (l == it.bx && it.by == refY) {
//          l + 1 to r
//        } else if (r == it.bx && it.by == refY) {
//          l to r - 1
//        } else {
//          inter
//        }
        inter
      } else {
        null
      }
    }

    println(SegmentUnion(points).length())
  }

  fun part2() {
    val input = DataReader.read(15)
    // println("Result is $result")
  }

  // represents a line whose equation is ay + bx + c = 0
  data class Line(val a: Int, val b: Int, val c: Int) {
    companion object {
      fun betweenPoints(b: Point, c: Point): Line {
        val aa = b.x - c.x
        val bb = b.y - c.y
        val cc = b.y * (b.y - c.y) - b.x * (b.x - c.x)
        return Line(aa, bb, cc)
      }
    }

    fun intersect(l2: Line): Point {
      val l1 = this
      val x = (l1.b * l2.c - l2.b * l1.c) / (l1.a * l1.b - l2.a * l2.b)
      val y = (l2.a * l1.c - l1.a * l2.c) / (l1.a * l2.b - l2.a * l1.b)
      return Point(x, y)
    }

    fun str(): String {
      return "$a * x ${if (b < 0) '-' else '+'}${abs(b)} * y ${if (b < 0) '-' else '+'} ${abs(c)} = 0"
    }
  }

  class SegmentUnion(val segments: List<Pair<Int, Int>>) {
    fun length(): Int {
      val points = segments.asSequence()
        .flatMap { sequenceOf((it.first to 'L'), (it.second to 'R')) }
        .iterator()
        .let { iter ->
          Array(segments.size * 2) {
            iter.next()
          }
        }
        .sortedWith(compareBy<Pair<Int, Char>> { it.first }.thenBy { it.second })

      var counter = 0
      var answer = 0

      for (i in points.indices) {
        if (counter != 0) {
          answer += points[i].first - points[i - 1].first
        }
        if (points[i].second == 'R') {
          counter--
        } else {
          counter++
        }
      }
      println(points)

      return answer
    }
  }

  data class Point(val x: Int, val y: Int)
}
