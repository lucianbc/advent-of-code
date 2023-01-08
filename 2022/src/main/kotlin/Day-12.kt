import java.util.PriorityQueue

fun main() {
  Day12.part2()
}

object Day12 {
  fun part1() {
    val input = DataReader.read(12).read2dMap()

//    val distances = Array(input.size) { row -> Array(input[row].size) { unvisited } }
    val (sl, sc) = input.find('S')
    val (el, ec) = input.find('E')

//    distances[el][ec] = 0

    input[sl][sc] = 'a'
    input[el][ec] = 'z'

//    val result = runFind(input, distances, sl, sc)

    val distances = runFind(input, sl, sc)
    val result = distances[el][ec]

    println("Result is $result")

//    distances.zip(input).forEach { (distanceLine, heightLine) ->
//      val l = distanceLine.zip(heightLine.toList()).map { (dNum, hChar) -> "(%2d $hChar)".format(dNum) }.joinToString(", ")
//      println(l)
//    }

//    distances.forEach {
//      println(it.joinToString())
//    }
  }

  fun runFind(map: Array<CharArray>, startLine: Int, startCol: Int): Array<Array<Int>> {
    fun getNeighbors(node: Pair<Int, Int>) = with(node) {
      listOf(
        first + 1 to second,
        first - 1 to second,
        first to second + 1,
        first to second - 1,
      ).filter {
        if (map.indices.contains(it.first) && map[0].indices.contains(it.second)) {
          val crtHeight = map[it.first][it.second]
          val prevHeight = map[first][second]
          crtHeight.code <= prevHeight.code + 1
        } else {
          false
        }
      }
    }

    val distances = Array(map.size) { line -> Array(map[line].size) { _ -> Int.MAX_VALUE } }
    val queue = PriorityQueue<Pair<Int, Int>> { a, b ->
      val dA = distances[a.first][a.second]
      val dB = distances[b.first][b.second]
      dA.compareTo(dB)
    }

    distances[startLine][startCol] = 0
    queue.add(startLine to startCol)

    while (queue.isNotEmpty()) {
      val crtElement = queue.poll()!!
      val neighbors = getNeighbors(crtElement)
      neighbors.forEach { n ->
        val dn = distances[n.first][n.second]
        if (dn > distances[crtElement.first][crtElement.second] + 1) {
          distances[n.first][n.second] = distances[crtElement.first][crtElement.second] + 1
          queue.add(n)
        }
      }
    }
    return distances
  }



  fun runFind(map: Array<CharArray>, distances: Array<Array<Int>>, startLine: Int, startCol: Int): Int {
    fun find(crtLine: Int, crtCol: Int, prevLine: Int, prevCol: Int): Int {
//      println("Run here $crtLine $crtCol")
      if (!distances.indices.contains(crtLine) || !distances[0].indices.contains(crtCol)) {
//        println("x")
        return Int.MAX_VALUE
      }
      val crtHeight = map[crtLine][crtCol]
      val prevHeight = map[prevLine][prevCol]
//      println("Compare $crtHeight - ${crtHeight.code} to $prevHeight - ${prevHeight.code}")
//      if (crtHeight.code < prevHeight.code) {
//        return Int.MAX_VALUE
//      }
      if (crtHeight.code > prevHeight.code + 1) {
//        println("y")
        return Int.MAX_VALUE
      }
      if (distances[crtLine][crtCol] == -2) {
//        println("z")
        return Int.MAX_VALUE
      }
      /*
      Sabqponm
      abcryxxl
      accszExk
      acctuvwj
      abdefghi

        v..v<<<<
        >v.vv<<^
        .>vv>E^^
        ..v>>>^^
        ..>>>>>^
       */


      if (distances[crtLine][crtCol] >= 0) {
        return distances[crtLine][crtCol]
      }
      distances[crtLine][crtCol] = -2
      val dist = listOf(
        find(crtLine - 1, crtCol, crtLine, crtCol),
        find(crtLine + 1, crtCol, crtLine, crtCol),
        find(crtLine, crtCol - 1, crtLine, crtCol),
        find(crtLine, crtCol + 1, crtLine, crtCol),
      ).min() + 1
      distances[crtLine][crtCol] = dist
      return dist
    }
    return find(startLine, startCol, startLine, startCol)
  }

  fun part2() {
    val input = DataReader.read(12).read2dMap()
    val (el, ec) = input.find('E')
    val (sl, sc) = input.find('S')

    input[sl][sc] = 'a'
    input[el][ec] = 'z'

    val r = input.findAll('a').map {
      val d = runFind(input, it.first, it.second)
      d[el][ec]
    }

    println("Result is ${r.min()}")
  }
}
