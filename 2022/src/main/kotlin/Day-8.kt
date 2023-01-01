fun main() {
  Day8.part2()
}

object Day8 {

  data class Tree(val row: Int, val col: Int, val height: Int)

  fun detectVisible(trees: Array<Tree>): Set<Tree> {
    var maxSoFar = trees.first().height
    val visible = mutableSetOf(trees.first())

    trees.forEach {
      if (it.height > maxSoFar) {
        visible.add(it)
        maxSoFar = it.height
      }
    }
    return visible
  }

  /*
  (0,0) (0,1) (0,2)
  (1,0) (1,1) (1,2)


  (0,2) (1,2)    (0,0) -> (0,2)
  (0,1) (1,1)    (0,1) -> (1,2)
  (0,0) (1,0)    (1,0) -> (0,1)
                 (1,1) -> (1,1)
                 (2,0) -> (0,0)
                 (2,1) -> (1,0)

                 (line, column) -> (column, totalLines - line)
  */
  fun rotate(trees: Array<Array<Tree>>): Array<Array<Tree>> {
    val transposedLineNum = trees[0].size

    val newArr = Array(transposedLineNum) { line ->
      Array(trees.size) { column ->
        trees[column][transposedLineNum - line - 1]
      }
    }

    return newArr
  }

  fun takeAbove(trees: Array<Array<Tree>>, row: Int, col: Int): Array<Tree> {
    return (row downTo 0).map { r ->
      trees[r][col]
    }.toTypedArray()
  }

  fun takeBelow(trees: Array<Array<Tree>>, row: Int, col: Int): Array<Tree> {
    return (row until trees.size).map { r -> trees[r][col] }
      .toTypedArray()
  }

  fun takeLeft(trees: Array<Array<Tree>>, row: Int, col: Int): Array<Tree> {
    return (col downTo 0).map { c -> trees[row][c] }.toTypedArray()
  }

  fun takeRight(trees: Array<Array<Tree>>, row: Int, col: Int): Array<Tree> {
    return (col until trees[row].size).map { c -> trees[row][c] }.toTypedArray()
  }

  fun readMap(): Array<Array<Tree>> {
    return DataReader.read(8)
      .mapIndexed { index, it ->
        Array(it.length) { pos -> Tree(index, pos, it[pos].digitToInt()) }
      }
      .toTypedArray()
  }

  fun part1() {
    val result = readMap()

    val v1 = result.map { detectVisible(it) }
    val v2 = result.map {
      it.reversedArray().let(::detectVisible)
    }

    val rotated = rotate(result)

    val v3 = rotated.map(::detectVisible)
    val v4 = rotated.map { it.reversedArray().let(::detectVisible) }

    val visibleAll = sequenceOf(v1, v2, v3, v4).flatMap { it }
      .fold(emptySet<Tree>()) { acc, crt ->
        acc + crt
      }

    println("Result is ${visibleAll.size}")
  }

  fun countVisible(array: Array<Tree>): Int {
    if (array.size == 1) {
      return 0
    }

    var count = 0
    for (it in array.drop(1)) {
      count++
      if (it.height >= array[0].height) {
        break
      }
    }

    return count
  }

  fun part2() {
    val data = readMap()

    val maxScore = data.indices.flatMap { line ->
      data[line].indices.map { col ->
        sequenceOf(
          takeLeft(data, line, col),
          takeRight(data, line, col),
          takeAbove(data, line, col),
          takeBelow(data, line, col),
        ).map {
          countVisible(it)
        }.fold(1) { acc, crt -> acc * crt }
      }
    }.max()


    println("Result is $maxScore")
  }
}
