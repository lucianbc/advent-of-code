import java.util.LinkedList

fun main() {
  Day7.part2()
}

object Day7 {
  fun parseInput(): Node.Directory {
    val dataIterator = DataReader.read(7).listIterator()
    val root = Node.Directory(null, "/")
    var current = root
    dataIterator.next()

    while (dataIterator.hasNext()) {
      val currentCommand = dataIterator.next()
      if (currentCommand == "$ ls") {
        val contents = mutableListOf<Node>()
        while (dataIterator.hasNext()) {
          val crtLine = dataIterator.next()
          if (crtLine.startsWith("$")) {
            dataIterator.previous()
            break
          } else {
            if (crtLine.startsWith("dir")) {
              val dirName = crtLine.split(" ")[1]
              contents.add(Node.Directory(current, dirName))
            } else {
              val fileRaw = crtLine.split(" ")
              contents.add(Node.File(current, fileRaw[0].toInt(), fileRaw[1]))
            }
          }
        }
        current.children.addAll(contents)
      } else if (currentCommand.startsWith("$ cd")) {
        val cdRaw = currentCommand.split(" ")
        val dest = cdRaw[2]
        current = if (dest == "..") {
          current.parent!!
        } else {
          current.children.find { it.name == dest } as Node.Directory
        }
      }
    }

    return root
  }

  sealed interface Node {
    val size: Int
    val name: String
    data class Directory(
      val parent: Directory?,
      override val name: String,
      val children: MutableList<Node> = mutableListOf(),
    ): Node {
      override val size: Int
        get() {
          return children.sumOf { it.size }
        }
    }

    data class File(
      val parent: Directory,
      override val size: Int,
      override val name: String
    ): Node
  }

  fun part1() {
    val data = parseInput()

    val stack = LinkedList<Node.Directory>()

    stack.addLast(data)

    val dirs = mutableListOf<Node.Directory>()

    while (stack.isNotEmpty()) {
      val crt = stack.removeLast()!!
      val crtSize = crt.size

      if (crtSize <= 100000) {
        dirs.add(crt)
      }

      crt.children.forEach {
        when (it) {
          is Node.Directory -> {
            stack.addLast(it)
          }
          is Node.File -> {
            // do nothing
          }
        }
      }
    }

    val result = dirs.sumOf { it.size }
    println("Result is $result")
  }

  fun Node.Directory.iterate(): Sequence<Node.Directory> {
    return sequence {
      yield(this@iterate)
      this@iterate.children.forEach {
        if (it is Node.Directory) {
          yieldAll(it.iterate())
        }
      }
    }
  }

  fun part2() {

    val totalDiskSpace = 70000000
    val requiredFreeSpace = 30000000

    val data = parseInput()

    val freeSpace = totalDiskSpace - data.size

    val requiredSpace = requiredFreeSpace - freeSpace

    var dirToDelete: Node.Directory = data

    data.iterate().forEach {
      if (it.size >= requiredSpace && it.size < dirToDelete.size) {
        dirToDelete = it
      }
    }

    println("Result is ${dirToDelete.size}")
  }
}
