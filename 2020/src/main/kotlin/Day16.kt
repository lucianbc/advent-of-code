fun main() {
    Day16.part2()
//    Day16.Field(0 to 1, 4 to 19)
}

object Day16 {
    data class Input(
        val fields: Map<String, Field>,
        val yourTicket: List<Int>,
        val nearbyTickets: List<List<Int>>
    )
    
    data class Field(val first: Pair<Int, Int>, val second: Pair<Int, Int>)
    
    val fieldRegex = "([a-z ]*): (\\d*)-(\\d*) or (\\d*)-(\\d*)".toRegex()

    fun parseField(line: String) = fieldRegex.find(line)!!.destructured
    
    fun parseTicket(line: String) = line.split(",").map(String::toInt)
    
    fun read(): Input {
        val data = DataReader.read(16)
        val iterator = data.iterator()
    
        val fields = mutableMapOf<String, Field>()
        while (iterator.hasNext()) {
            val current = iterator.next()
            if (current == "") {
                break
            }
            val (name, l1, h1, l2, h2) = parseField(current)
            fields[name] = Field(l1.toInt() to h1.toInt(), l2.toInt() to h2.toInt())
        }
    
        iterator.next() // skip your ticket
    
        val ticket = iterator.next().let(::parseTicket)
    
        iterator.next()
        iterator.next()
        
        val otherTickets = iterator.asSequence().map { it.let(::parseTicket) }.toList()
    
        return Input(fields, ticket, otherTickets)
    }
    
    class numberInInterval(data: Input) {
        val sortedIntervals = data.fields.entries
            .asSequence()
            .map { it.value }
            .flatMap { sequenceOf(it.first, it.second) }
            .sortedBy { it.first }
            .toList()
        
        fun apply(number: Int): Boolean {
            val fittingIntervals = sortedIntervals
                .firstOrNull { it.first <= number && it.second >= number }
            return fittingIntervals != null
        }
    }
    
    fun part1() {
        val data = read()
        
        val isNumberInAnyInterval = numberInInterval(data)
        
        val s = data.nearbyTickets
            .asSequence()
            .flatMap { it.asSequence() }
            .filter { !isNumberInAnyInterval.apply(it) }
            .sum()
        
        println("Sum of numbers not in any interval is $s")
    }
    
    fun part2() {
        val data = read()
    
        val isNumberInAnyInterval = numberInInterval(data)
        
        // filter out invalid tickets
        val validTickets = data.nearbyTickets
            .filter { it.map { t -> isNumberInAnyInterval.apply(t) }.reduce { a, b -> a && b } }
        
        val notMatchedSet = data.fields.keys.toMutableSet()
        
        val positionRange = validTickets.drop(1).fold(validTickets.first().map { listOf(it) }) { acc, crt ->
            acc.zip(crt).map { (list, el) -> list + listOf(el) }
        }

        val matchingTable = List(positionRange.size) { mutableSetOf<String>() }
        
        while (notMatchedSet.isNotEmpty()) {
            // assign pending classes all possible ticket ranges
            notMatchedSet.forEach { currentClass ->
                val currentField = data.fields[currentClass] ?: error("Field for class $currentClass not found")
                // add the current class to all ticket ranges that can be hold by the class
                positionRange.withIndex().forEach { (index, ticketRange) ->
                    if (currentField.canHold(ticketRange)) {
                        matchingTable[index].add(currentClass)
                    }
                }
            }
            
            // find the 1-to-1 mappings and remove them
            val exactMatches = matchingTable.flatMap {
                if (it.size == 1) {
                    listOf(it.first())
                } else emptyList()
            }
            
            // remove the class from ranges that can be matched to other classes
            matchingTable.forEach {
                if (it.size > 1) {
                    it.removeIf { el -> exactMatches.contains(el) }
                }
            }
            
            notMatchedSet.removeAll(exactMatches)
        }
        
        matchingTable
            .map { it.first() }
            .zip(data.yourTicket)
            .also { println("Full ticket: $it") }
            .filter { it.first.startsWith("departure") }
            .also { println("Dep ticket: $it") }
            .fold(1L) { acc, crt -> acc * crt.second }
            .also { println("Answer is $it") }
        
    }
    
    fun Field.canHold(range: Pair<Int, Int>) = this.first.includes(range) || this.second.includes(range)
    
    fun Field.canHold(positions: List<Int>) = positions
        .map { this.first.includes(it) || this.second.includes(it) }
        .reduce { a, b -> a && b }
    
    fun Pair<Int, Int>.includes(other: Pair<Int, Int>) = this.first <= other.first && this.second >= other.second
    
    fun Pair<Int, Int>.includes(other: Int) = this.first <= other && this.second >= other
    
    fun incorporateIntoRange(ticket: List<Int>, rangeSoFar: List<Pair<Int, Int>>) =
        ticket.zip(rangeSoFar).map { (place, range) ->
            when {
                place < range.first -> place to range.second
                place > range.second -> range.first to place
                else -> range
            }
        }
    
    fun ticketToRange(ticket: List<Int>) = ticket.map { it to it }
}
