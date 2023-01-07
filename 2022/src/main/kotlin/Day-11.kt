import java.math.BigInteger
import java.util.LinkedList

fun main() {
  Day11.part2()
}

typealias WorryLevel = BigInteger

fun String.toWorryLevel(): WorryLevel {
  return BigInteger(this)
}

fun Int.toWorryLevel(): WorryLevel {
  return BigInteger("$this")
}

object Day11 {

  data class Monkey(
    val id: Int,
    val items: MutableList<WorryLevel>,
    val divisibleByTest: WorryLevel,
    val operation: Operation,
    val monkeyIfTrue: Int,
    val monkeyIfFalse: Int,
  ) {
    var inspectCount = 0L
      private set
    fun inspect(monkeySystem: Map<Int, Monkey>) {
      items.forEach {
        inspectCount++
        val worry = operation.evaluate(it) / 3.toWorryLevel()
        val throwTo = if (worry % divisibleByTest == 0.toWorryLevel()) monkeyIfTrue else monkeyIfFalse
        monkeySystem[throwTo]!!.items.add(worry)
      }
      items.clear()
    }

    fun inspect2(monkeySystem: Map<Int, Monkey>) {
//      println("Monkey $id starts with $items")
      items.forEach {
        inspectCount++
        val worry = operation.evaluate(it) % 9699690.toWorryLevel()
        val throwTo = if (worry % divisibleByTest == 0.toWorryLevel()) monkeyIfTrue else monkeyIfFalse
        monkeySystem[throwTo]!!.items.add(worry)
      }
      items.clear()
    }
  }

  fun nextIteration(crtIteration: Map<Int, Monkey>, isPart2: Boolean = false): Map<Int, Monkey> {
    crtIteration.keys.sorted().forEach { monkeyId ->
      val crtMonkey = crtIteration[monkeyId]!!
      if (isPart2) {
        crtMonkey.inspect2(crtIteration)
      } else {
        crtMonkey.inspect(crtIteration)
      }
    }

    return crtIteration
  }

  fun parse(input: List<String>): Map<Int, Monkey> {
    val monkeySystem = mutableMapOf<Int, Monkey>()
    input.splitBy("").forEach {
      val monkeyId = "Monkey (\\d+):".toRegex().matchEntire(it[0])!!.groups[1]!!.value.toInt()
      val startingItems = "\\s+Starting items: (.*)".toRegex().matchEntire(it[1])!!.groupValues[1].split(", ").map(String::toWorryLevel).let { LinkedList(it) }
      val operation = "\\s+Operation: new = (.*)".toRegex().matchEntire(it[2])!!.groupValues[1].let(Operation.Companion::parse)
      val divisibleByTest = "\\s+Test: divisible by (.*)".toRegex().matchEntire(it[3])!!.groupValues[1].toWorryLevel()
      val monkeyIfTrue = "\\s+If true: throw to monkey (.*)".toRegex().matchEntire(it[4])!!.groupValues[1].toInt()
      val monkeyIfFalse = "\\s+If false: throw to monkey (.*)".toRegex().matchEntire(it[5])!!.groupValues[1].toInt()

      monkeySystem[monkeyId] = Monkey(monkeyId, startingItems, divisibleByTest, operation, monkeyIfTrue, monkeyIfFalse)
    }
    return monkeySystem
  }

  sealed interface Operation {
    data class Product(override val lhs: Term, override val rhs: Term) : Operation
    data class Sum(override val lhs: Term, override val rhs: Term) : Operation

    val lhs: Term
    val rhs: Term

    companion object {
      fun parse(string: String): Operation {
        val groups = "(.*) ([\\+\\*]) (.*)".toRegex().matchEntire(string)!!.groupValues
        val lhs = Term.parse(groups[1])
        val rhs = Term.parse(groups[3])
        return if (groups[2] == "+") Sum(lhs, rhs) else Product(lhs, rhs)
      }
    }

    fun evaluate(old: WorryLevel): WorryLevel {
      return when (this) {
        is Product -> lhs.evaluate(old) * rhs.evaluate(old)
        is Sum -> lhs.evaluate(old) + rhs.evaluate(old)
      }
    }
  }

  sealed interface Term {
    data class Constant(val value: WorryLevel) : Term
    object Old : Term

    fun evaluate(old: WorryLevel): WorryLevel {
      return when(this) {
        is Constant -> this.value
        Old -> old
      }
    }

    companion object {
      fun parse(string: String): Term {
        return if (string == "old") Old else Constant(string.toWorryLevel())
      }
    }
  }

  fun part1() {
    val input = DataReader.read(11)

    val initialMonkeySystem = parse(input)

    val resultSystem = (1 .. 20).fold(initialMonkeySystem) { acc, iter ->
      println("Start iteration $iter")
      nextIteration(acc)
    }

    resultSystem.values.forEach { monkey ->
      println("Monkey ${monkey.id} -> inspected ${monkey.inspectCount}")
    }

    val result = resultSystem.values.map { monkey -> monkey.inspectCount }.sortedDescending().take(2).reduce { a, b -> a * b }

    // Result it 316888
    println("Result is $result")
  }

  fun part2() {
    val input = DataReader.read(11)

    val initialMonkeySystem = parse(input)

    val resultSystem = (1 .. 10000).fold(initialMonkeySystem) { acc, iter ->
//      println("Start iteration $iter")
      nextIteration(acc, true)
    }
    resultSystem.values.forEach { monkey ->
      println("Monkey ${monkey.id} -> inspected ${monkey.inspectCount}")
    }

    val r = resultSystem.map { it.value.divisibleByTest }.reduce { a, b -> a * b}
    println("WL: $r")

    val result = resultSystem.values.map { monkey -> monkey.inspectCount }.sortedDescending().take(2).reduce { a, b -> a * b }

    // Result it 316888
    println("Result is $result")
  }
}
