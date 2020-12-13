object DataReader {
    fun read(dayNr: Int) = readWhole(dayNr)
        .lines()
    
    fun readWhole(dayNr: Int) = javaClass.classLoader.getResource("inputs/day-$dayNr.txt")
        .readText()
}