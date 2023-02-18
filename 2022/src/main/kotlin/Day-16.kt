import java.lang.Integer.max

fun main() {
  Day16.part1()
}

object Day16 {
  data class Node(
    val name: String,
    val flow: Int,
    val links: List<String>
  )

  class GraphMap {
    val nodes = mutableMapOf<String, Node>()

    var firstNode = ""
    fun parseNode(inputString: String) {
      val result = "Valve ([A-Z]+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)".toRegex()
        .extractGroupsFromString(inputString)

      with(result) {
        val nodeKey = groups[1]!!.value
        val flowRate = groups[2]!!.value.toInt()
        val links = groups[3]!!.value.split(", ")

        val node = Node(nodeKey, flowRate, links)
        nodes[nodeKey] = node

        if (firstNode == "") {
          firstNode = nodeKey
        }
      }
    }

    private val memo = mutableMapOf<Pair<String, Int>, Int>()

    data class MemoKey(
      val nodeKey: String,
      val timeLeft: Int,
    )

    private val memo2 = mutableMapOf<MemoKey, Int>()

    fun maxScore2(nodeKey: String, timeLeft: Int, openedSoFar: List<String>, pathSoFar: List<String>): Int {
      println("Computing $nodeKey $timeLeft $openedSoFar $pathSoFar")
      val memoKey = MemoKey(
        nodeKey,
        timeLeft,
      )

      if (memo2.containsKey(memoKey)) {
        return memo2[memoKey]!!
      }

      val result = if (timeLeft <= 1) {
        0
      } else {
        val node = nodes[nodeKey]!!

        val nodeFlow = (timeLeft - 1) * node.flow

        val maxWithoutOpeningThis = node.links
          .filter { pathSoFar.contains(it).not() }
          .maxOfOrNull { maxScore2(it, timeLeft - 1, openedSoFar, pathSoFar + listOf(nodeKey)) } ?: if (openedSoFar.contains(nodeKey)) 0 else nodeFlow

        if (openedSoFar.contains(node.name)) {
          maxWithoutOpeningThis
        } else {
          val maxWithOpeningThis = node.links.map { maxScore2(it, timeLeft - 2, openedSoFar + listOf(nodeKey), emptyList()) }.max() + nodeFlow
          max(maxWithoutOpeningThis, maxWithOpeningThis)
        }
      }
      memo2[memoKey] = result
      return result
    }

    fun maxScore(nodeKey: String, timeLeft: Int): Int {
      val memoKey = nodeKey to timeLeft
//      println("Running for $nodeKey and $timeLeft")
      if (memo.containsKey(memoKey)) {
        return memo[memoKey]!!
      }

      val result = if (timeLeft <= 1) {
        0
      } else {
        nodes[nodeKey]!!.let { node ->
          val links = node.links
          val maxWithoutThis = links.maxOfOrNull { maxScore(it, timeLeft - 1) }!!

          if (node.flow == 0) {
            maxWithoutThis
          } else {
            val scoreOfThis = node.flow * (timeLeft - 1)
            val maxWithThis = links.maxOfOrNull { maxScore(it, timeLeft - 2) }!!

            if (scoreOfThis + maxWithThis > maxWithThis) {
              println("Open ${node.name} for ${timeLeft - 1} minutes")
            }

            max(maxWithThis, maxWithThis + scoreOfThis)
          }
        }
      }

      memo[memoKey] = result
      return result
    }
  }

  fun part1() {
    val input = DataReader.read(16)
    val graph = GraphMap()
    input.forEach {
      graph.parseNode(it)
    }
    println(graph.maxScore2(graph.firstNode, 30, emptyList(), emptyList()))
    // println("Result is $result")
  }

  fun part2() {
    val input = DataReader.read(16)
    // println("Result is $result")
  }
}
