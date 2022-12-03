object DataReader {
  fun read(name: String) = javaClass.classLoader.getResource("inputs/$name.txt")!!
    .readText().lines()

  fun read(dayNr: Int) = readWhole(dayNr)
    .lines()

  fun readWhole(dayNr: Int) = javaClass.classLoader.getResource("inputs/day-$dayNr.txt")!!
    .readText()
}