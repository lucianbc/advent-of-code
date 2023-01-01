fun main() {
  Day10.part2()
}

object Day10 {
  data class RegisterValue(val cycle: Int, val beginning: Int, val end: Int)

  object Computer {
    var cycle = 0
    var X = 1

    fun execute(instruction: String): Sequence<RegisterValue> {
      return sequence {
        if (instruction == "noop") {
          cycle += 1
          yield(RegisterValue(cycle, X, X))
        } else {
          val toAdd = instruction.split(" ")[1].toInt()
          cycle += 1
          yield(RegisterValue(cycle, X, X))
          val start = X
          cycle += 1
          X += toAdd
          yield(RegisterValue(cycle, start, X))
        }
      }
    }

    fun executeInstructions(instructions: List<String>): List<RegisterValue> {
      return instructions.flatMap { execute(it) }
    }

    fun render(instructions: List<String>) {
      val registerValues = executeInstructions(instructions)
      val screen = Array(6) { _ -> Array(40) { _ -> '.'} }

      for (r in registerValues) {
        val line = (r.cycle - 1) / 40
        val col = (r.cycle - 1) % 40

        val spriteBegin = r.beginning - 1
        if (col == spriteBegin || col == spriteBegin + 1 || col == spriteBegin + 2) {
          screen[line][col] = '#'
        }
      }

      println(screen.join())
    }
  }

  fun part1() {
    val input = DataReader.read(10)

    val x = Computer.executeInstructions(input)
      .filter { it.cycle in setOf(20, 60, 100, 140, 180, 220) }
      .map { it.cycle * it.beginning }
      .sum()

    println("Result is $x")
  }

  fun part2() {
    val input = DataReader.read(10)
    Computer.render(input)
    // PZGPKPEB
//    println("Result is $result")
  }
}
