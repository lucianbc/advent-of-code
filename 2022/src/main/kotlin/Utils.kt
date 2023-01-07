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