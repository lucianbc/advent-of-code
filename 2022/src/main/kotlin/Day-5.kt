import java.util.LinkedList

fun main() {
  Day5.part2()
}

object Day5 {
  fun part1() {
    val (stacks, commands) = readData()

    commands.forEach { command ->
      val (count, from, to) = command
      val toStack = stacks[to]
      val fromStack = stacks[from]
      for (i in 0 until count) {
        val crtCrate = fromStack.removeLast()
        toStack.addLast(crtCrate)
      }
    }

    val result = stacks.readResult()

    println("Result is $result")
  }

  fun List<List<Char>>.readResult(): String {
    return map { stack ->
      if (stack.size == 0) {
        ' '
      } else {
        stack.last()
      }
    }.joinToString("")
  }

  fun part2() {
    val (stacks, commands) = readData()

    commands.forEach { command ->
      val (count, from, to) = command
      val toStack = stacks[to]
      val fromStack = stacks[from]
      val toMove = fromStack.takeLast(count).toMutableList()

      for (i in 0 until count) {
        val crtCrate = fromStack.removeLast()
        toStack.addLast(toMove.first())
        toMove.removeFirst()
      }
    }

    val result = stacks.readResult()

    println("Result is $result")
  }

  fun readData(): Pair<List<LinkedList<Char>>, MutableList<Triple<Int, Int, Int>>> {
    val lines = DataReader.read(5)
    val iterator = lines.iterator()
    var crtLine = iterator.next()

    val numOfStacks = (crtLine.length - 3) / 4 + 1

    val stacks = List(numOfStacks) {
      LinkedList<Char>()
    }

    while (!crtLine.startsWith(" 1 ")) {
      var crtLineIndex = 0
      var stackIndex = 0
      while(crtLineIndex < crtLine.length) {
        val element = crtLine[crtLineIndex + 1]
        if (element != ' ') {
          stacks[stackIndex].addFirst(element)
        }
        crtLineIndex += 4
        stackIndex += 1
      }
      crtLine = iterator.next()
    }
    iterator.next()

    val commands = mutableListOf<Triple<Int, Int, Int>>()

    while (iterator.hasNext()) {
      crtLine = iterator.next()
      val arr = crtLine.split(" ")
      val count = arr[1].toInt()
      val from = arr[3].toInt()
      val to = arr[5].toInt()
      commands.add(Triple(count, from - 1, to - 1))
    }

    return stacks to commands
  }
}


// 4 * x + 3 = len => 4*x = len-3 => x = (len - 3) / 4