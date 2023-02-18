fun Regex.extractGroupsFromString(inputString: String): MatchResult {
  return matchEntire(inputString)
    .apply { require(this != null) { "Input $inputString does not match the expected regex" } }!!
}

fun Array<Array<Char>>.join(): String {
  return this.joinToString("\n") { it.joinToString("") }
}

fun List<String>.groupWithSeparator(separator: String): List<List<String>> {
  val result = mutableListOf<List<String>>()
  var crtGroup = mutableListOf<String>()

  forEach { crtLine ->
    if (crtLine == separator) {
      if (crtGroup.isNotEmpty()) {
        result.add(crtGroup)
        crtGroup = mutableListOf()
      } else {
        crtGroup.add(crtLine)
      }
    }
  }
  if (crtGroup.isNotEmpty()) {
    result.add(crtGroup)
  }
  return result
}

fun List<String>.read2dMap(): Array<CharArray> {
  return this.map { it.toCharArray() }.toTypedArray()
}

fun Array<CharArray>.find(char: Char): Pair<Int, Int> {
  this.indices.forEach { line ->
    this[line].indices.forEach { col ->
      if (this[line][col] == char) {
        return line to col
      }
    }
  }
  return -1 to -1
}

fun Array<CharArray>.findAll(char: Char): Sequence<Pair<Int, Int>> {
  return sequence {
    indices.forEach { line ->
      this@findAll[line].indices.forEach { col ->
        if (this@findAll[line][col] == char) {
          yield(line to col)
        }
      }
    }
  }
}