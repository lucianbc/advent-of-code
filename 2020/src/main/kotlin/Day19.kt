fun main() {
//    Day19.Parser.parseRule("0: 2 1 3 | 4 2").let { println(it) }
//    Day19.Parser.parseRule("1: \"a\"").let { println(it) }

//    Day19.readData().also {
//        println(it.first)
//        println(it.second.toList())
//    }
    
    Day19.part1()
}

object Day19 {
    fun part1() {
        readData().also { (rules, strings) ->
            val rule = rules[0] ?: error("Rule 0 not found")
            val count = strings.map {
                it to when (val match = rule.matches(rules, it, 0)) {
                    is Matches.Yes -> match.nextPosition == it.length
                    Matches.No -> false
                }
            }
                .count { it.second }
            println("Count is $count")
        }
    }
    
    fun part2() {
        readData("day-19-pt2").also { (rules, strings) ->
            val rule = rules[0] ?: error("Rule 0 not found")
            val count = strings.map {
                it to when (val match = rule.matches(rules, it, 0)) {
                    is Matches.Yes -> match.nextPosition == it.length
                    Matches.No -> false
                }
            }
                .count { it.second }
            println("Count is $count")
        }
    }
    
    sealed class Rule {
        data class SingleChar(val expectedChar: Char): Rule()
        data class ListOfRules(val list: List<Int>): Rule()
        data class Disjunction(val lists: List<ListOfRules>): Rule()
    }
    
    sealed class Matches {
        data class Yes(val nextPosition: Int): Matches()
        object No: Matches()
    }
    
    fun Rule.matches(context: Map<Int, Rule>, string: String, startAt: Int): Matches {
        return when (this) {
            is Rule.SingleChar ->
                if (startAt < string.length && string[startAt] == expectedChar) Matches.Yes(startAt + 1)
                else Matches.No
            is Rule.ListOfRules ->
                list.asSequence()
                    .map { context[it] ?: error("Rule with id $it not found") }
                    .fold(Matches.Yes(startAt) as Matches) { acc, crt ->
                        when (acc) {
                            is Matches.No -> acc
                            is Matches.Yes -> crt.matches(context, string, acc.nextPosition)
                        }
                    }
            is Rule.Disjunction ->
                lists.asSequence()
                    .map { it.matches(context, string, startAt) }
                    .find { when(it) {
                        is Matches.Yes -> true
                        Matches.No -> false
                    } }
                    .let { it ?: Matches.No }
        }
    }
    
    fun readData(name: String? = null): Pair<Map<Int, Rule>, Sequence<String>> {
        val iterator = name?.let { DataReader.read(name).iterator() } ?: DataReader.read(19).iterator()
        val ruleMap = mutableMapOf<Int, Rule>()
        var hasFinishedRules = false
        
        while (iterator.hasNext() && !hasFinishedRules) {
            val current = iterator.next()
            if (current == "")
                hasFinishedRules = true
            else {
                val entry = Parser.parseRule(current)
                ruleMap[entry.first] = entry.second
            }
        }
        
        return ruleMap to iterator.asSequence()
    }
    
    object Parser {
        private val headRegex = "(\\d+): ".toRegex()
        private val tailRegex = "(\\d+)|(\\|)".toRegex()
        private val singleCharRegex = "\"([a-z])\"".toRegex()
        
        fun parseRule(ruleLine: String): Pair<Int, Rule> {
            val idMatch = headRegex.find(ruleLine)!!
            val rest = ruleLine.substring(idMatch.range.last + 1)
            val (id) = idMatch.destructured
            
            val isSingleChar = singleCharRegex.find(ruleLine)
            
            val result = if (isSingleChar != null) {
                val (char) = isSingleChar.destructured
                Rule.SingleChar(char.first())
            } else {
                val (inProg, rules) = tailRegex.findAll(rest)
                    .fold(emptyList<Int>() to emptyList<Rule.ListOfRules>()) { (progress, finished), crt ->
                        val (number, _) = crt.destructured
                        if (number != "") {
                            progress + listOf(number.toInt()) to finished
                        } else {
                            emptyList<Int>() to finished + listOf(Rule.ListOfRules(progress))
                        }
                    }
    
                if (rules.isEmpty()) {
                    Rule.ListOfRules(inProg)
                } else {
                    Rule.Disjunction(
                        if (inProg.isEmpty()) rules else rules + listOf(Rule.ListOfRules(inProg))
                    )
                }
            }
            return id.toInt() to result
        }
    }
}
