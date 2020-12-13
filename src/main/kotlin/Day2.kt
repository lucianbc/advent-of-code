import java.lang.Exception

fun main() {
    val count = DataReader.read(2).asSequence()
        .map(::parseRecord)
        .count { it.isValid2() }
    
    println("Result: $count")
}

data class Record(val min: Int, val max: Int, val target: Char, val value: String)

fun Record.isValid() =
    value.count { it == target } in min..max

fun Record.isValid2() =
    (value[min - 1] == target) xor (value[max - 1] == target)

val regex = "(\\d+)-(\\d+) (.): (.*)".toRegex()
fun parseRecord(record: String): Record {
    val (min, max, char, line) = regex.find(record)?.destructured ?: throw Exception("Could not parse $record")
    return Record(min.toInt(), max.toInt(), char[0], line)
}