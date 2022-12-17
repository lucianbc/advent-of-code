fun main() {
  Day4.part2()
}

object Day4 {
  data class Interval(val left: Int, val right: Int) {
    fun fullyContains(other: Interval): Boolean {
      return this.left <= other.left && this.right >= other.right
    }

    // a b c d -> no
    // a c b d -> yes
    // a c d b -> yes
    // c a b d -> yes
    // c d a b -> no

    fun overlapsWith(other: Interval): Boolean {
      val (first, second) = if (this.left <= other.left) this to other else other to this

      return first.right >= second.left
    }
  }

  fun readPairs(): List<Pair<Interval, Interval>> {
    val pairs = DataReader.read(4)
      .map { it ->
        it.split(",").let {
          it[0].split("-").let { i1 ->
            Interval(i1[0].toInt(), i1[1].toInt())
          } to
              it[1].split("-").let { i2 ->
                Interval(i2[0].toInt(), i2[1].toInt())
              }
        }
      }
    return pairs
  }

  fun part1() {
    val pairs = readPairs()

    val ct = pairs.count { (i1, i2) ->
      i1.fullyContains(i2) || i2.fullyContains(i1)
    }

    println("Answer is ${ct}")
  }

  fun part2() {
    val pairs = readPairs()
    val ct = pairs.count { (i1, i2) ->
      i1.overlapsWith(i2)
    }
    println("Answer is $ct")
  }
}


