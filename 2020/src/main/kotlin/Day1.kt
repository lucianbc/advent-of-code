import javax.xml.crypto.Data

fun main() {
//    part1()
    part2()
}

private fun part1() {
    val sorted = FileReader.read()
    for (crt in sorted) {
        val result = sorted.find { it + crt == 2020 }
        if (result != null) {
            println("Found $crt and $result - ${crt * result}")
            return;
        }
    }
    println("No result found")
}

private fun part2() {
    val sorted = FileReader.read()
    for (crt1It in 0 until sorted.lastIndex - 1) {
        for (crt2It in crt1It until sorted.lastIndex) {
            val c1 = sorted[crt1It]
            val c2 = sorted[crt2It]
            val result = sorted.find { c1 + c2 + it == 2020 }
            if (result != null) {
                println("Found $c1 $c2 $result - ${c1 * c2 * result}")
                return
            }
        }
    }
    println("No result found")
}

object FileReader {
    fun read() =
        DataReader.read(1)
            .map { it.toInt() }
            .sorted()
}
