fun Array<Array<Char>>.join(): String {
  return this.joinToString("\n") { it.joinToString("") }
}