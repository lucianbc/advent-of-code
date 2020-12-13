import java.lang.Exception

fun main() {
    part2()
}

private fun part1() {
    parseAndValidate(containsRequired)
}

val containsRequired: (IdentityCard) -> Boolean = {
    it.keys.containsAll(setOf(
        "pid",
        "byr",
        "iyr",
        "eyr",
        "hgt",
        "hcl",
        "ecl",
    ))
}

private fun part2() {
    println("Reaches here")
    val criterias = mapOf<String, (String) -> Boolean>(
        "byr" to { (1920 until 2002).contains(it.toInt()) },
        "iyr" to { (2010 until 2020).contains(it.toInt()) },
        "eyr" to { (2020 until 2030).contains(it.toInt()) },
        "hgt" to {
            if (it.takeLast(2) == "cm")
                (150 until 193).contains(it.take(it.length - 2).toInt())
            else
                (59 until 76).contains(it.take(it.length - 2).toInt())
        },
        "hcl" to { it.length == 7 && it.first() == '#' },
        "ecl" to { setOf("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(it) },
        "pid" to { it.matches("[0-9]{9}".toRegex()) },
        "cid" to { true },
    )
    
    parseAndValidate {
        it.entries.fold(true) {
            acc, (key, value) ->
                acc && try {
                    criterias[key]?.invoke(value) ?: false
                } catch (_: Exception) {
                    false
                }
        }
    }
}

fun parseAndValidate(criteria: (IdentityCard) -> Boolean) {
    val whole = DataReader.readWhole(4)
    
    val result = parseString(whole)
    
    val count = result.count(criteria)
    
    println("Numb of valid ids is $count")
}

fun parseString(value: String): List<IdentityCard> {
    val regex = (
        "(pid:[^\\s]*)|" +
        "(byr:[^\\s]*)|" +
        "(iyr:[^\\s]*)|" +
        "(eyr:[^\\s]*)|" +
        "(hgt:[^\\s]*)|" +
        "(hcl:[^\\s]*)|" +
        "(ecl:[^\\s]*)|" +
        "(cid:[^\\s]*)|" +
        "(\n(?=\n))"
        ).toRegex()
    
    val all = regex.findAll(value)
    
    val elements = mutableListOf<IdentityCard>()
    
    var current = emptyIdCard()
    
    all.forEach {
        val matched = it.groupValues
        
        for (i in 1 .. 8) {
            if (matched[i].length > 4) {
                current[matched[i].substring(0, 3)] = matched[i].substring(4)
            }
        }
        
        if (matched[9] != EMPTY) {
            elements.add(current)
            current = emptyIdCard()
        }
    }
    
    if (current.isNotEmpty()) {
        elements.add(current)
    }
    
    return elements
}

typealias IdentityCard = Map<String, String>

fun emptyIdCard() = hashMapOf<String, String>()

const val EMPTY = ""
