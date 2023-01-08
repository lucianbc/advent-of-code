fun main() {
  Day13.part2()
}

object Day13 {

  sealed interface Node {
    data class Cons(val value: NodeData, val next: Node): Node
    object Nil: Node

    fun render(): String {
      if (this is Nil) {
        return "[]"
      } else {
        var crt: Node = this as Cons

        var strs = mutableListOf<String>()

        while (crt !is Nil) {
          crt as Cons
          strs.add(crt.value.render())
          crt = crt.next
        }
        return "[${strs.joinToString(",")}]"
      }
    }
  }

  sealed interface NodeData {
    data class Number(val value: Int) : NodeData {
      override fun render(): String {
        return "$value"
      }
    }
    data class List(val node: Node): NodeData {
      override fun render(): String {
        return node.render()
      }
    }

    fun toList(): List {
      return if (this is List) this else List(Node.Cons(this, Node.Nil))
    }

    fun render(): String
  }

  fun parse(charIterator: ListIterator<Char>): Node {
    if (!charIterator.hasNext()) {
      return Node.Nil
    }
    val head = charIterator.next()
    if (head == '[') {
      val inner = parse(charIterator)
      val node = NodeData.List(inner)
//      val endList = charIterator.next()
//      require(endList == ']') {
//        "List does not end correctly"
//      }
      val next = parse(charIterator)
      return Node.Cons(node, next)
    } else if (head == ',') {
      return parse(charIterator)
    } else if (head == ']') {
      return Node.Nil
    } else {
      val buffer = StringBuffer().append(head)
      while (charIterator.hasNext()) {
        val next = charIterator.next()
        if (next.isDigit()) {
          buffer.append(next)
        } else {
          charIterator.previous()
          break
        }
      }
      if (buffer.isEmpty()) {
        return Node.Nil
      }

      val number = buffer.toString().toInt()
      val numberNode = NodeData.Number(number)
      val next = parse(charIterator)
      return Node.Cons(numberNode, next)
    }
  }

  fun parse(s: String): NodeData.List {
    val node = parse(s.toList().listIterator())
    node as Node.Cons
    return node.value as NodeData.List
  }

  fun compareNodes(left: Node, right: Node): Int {
//    println("Compare ${left.render()} to ${right.render()}")
    return if (left is Node.Cons && right is Node.Cons) {
      compareCons(left, right)
    } else if (left is Node.Cons && right is Node.Nil) {
      1
    } else if (left is Node.Nil && right is Node.Cons) {
      -1
    } else {
      0
    }
  }

  fun compareCons(left: Node.Cons, right: Node.Cons): Int {
    val leftHead = left.value
    val rightHead = right.value

    return if (leftHead is NodeData.Number && rightHead is NodeData.Number) {
      if (leftHead.value < rightHead.value) {
        -1
      } else if (leftHead.value > rightHead.value) {
        1
      } else {
        compareNodes(left.next, right.next)
      }
    } else {
      val comp = compareNodes(leftHead.toList().node, rightHead.toList().node)
      return if (comp == 0) {
        compareNodes(left.next, right.next)
      } else {
        comp
      }
    }
  }

  fun part1() {
    val input = parseInput()

    val inOrder = input.mapIndexed { index, pair ->
      index to compareNodes(pair.first.node, pair.second.node)
    }.toList()

    println(inOrder)
    val result = inOrder.fold(0) { acc, crt ->
      acc + if (crt.second == -1) crt.first + 1 else 0
    }

    println()

     println("Result is $result")
  }

  fun parseInput() = DataReader.read(13)
    .splitBy("")
    .map {
      parse(it[0]) to parse(it[1])
    }

  fun part2() {

    val input = parseInput().flatMap { it.toList() }

    val div1 = parse("[[2]]")
    val div2 = parse("[[6]]")

    val sortedPackets = sequence {
      yieldAll(input)
      yield(div1)
      yield(div2)
    }.sortedWith { o1, o2 -> compareNodes(o1.node, o2.node) }.toList()

    val i1 = sortedPackets.indexOf(div1) + 1
    val i2 = sortedPackets.indexOf(div2) + 1

    val result = i1 * i2

    println("Result is $result")
  }
}
