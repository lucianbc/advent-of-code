import java.lang.Integer.max

fun main() {
  Day16.part2()
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

    data class MemoKeyPart22(
      val nodeKey: String,
      val timeLeft: Int,
      val openedSoFar: Set<String>,
      val remainingPlayers: Int,
    )

    private val memo2part22 = mutableMapOf<MemoKeyPart22, Int>()

    fun maxScore2(
      nodeKey: String,
      timeLeft: Int,
      openedSoFar: Set<String>,
      pathSoFar: Set<String>,
      remainingPlayers: Int
    ): Int {
      val memoKey = MemoKeyPart22(
        nodeKey,
        timeLeft,
        openedSoFar,
        remainingPlayers,
      )

      if (memo2part22.containsKey(memoKey)) {
        return memo2part22[memoKey]!!
      }


      // 24614453 iterations
      // correct response 2400
      println("Computing ${stepCount++} $nodeKey $timeLeft $openedSoFar $pathSoFar")

      if (timeLeft == 0) {
        return if (remainingPlayers > 0) {
          maxScore2("AA", 26, openedSoFar, emptySet(), remainingPlayers - 1)
        } else {
          0
        }
      }

      var result = 0

      val node = nodes[nodeKey]!!

      if (openedSoFar.contains(nodeKey).not() && node.flow != 0) {
        result = max(
          result,
          (timeLeft - 1) * node.flow + maxScore2(
            nodeKey,
            timeLeft - 1,
            openedSoFar + setOf(nodeKey),
            emptySet(),
            remainingPlayers
          )
        )
      }

      node.links.forEach { link ->
//        if (pathSoFar.contains(link).not()) {
          result = max(result, maxScore2(link, timeLeft - 1, openedSoFar, pathSoFar + setOf(link), remainingPlayers))
//        }
      }
      memo2part22[memoKey] = result
      return result
    }

    data class MemoPart2Key(
      val nodeKey: String,
      val otherNodeKey: String,
      val timeLeft: Int,
      val openedSoFar: Set<String>,
    )

    private val memoPart2 = mutableMapOf<MemoPart2Key, Int>()

    fun maxScorePart2(nodeKey: String, otherNodeKey: String, timeLeft: Int, openedSoFar: Set<String>, path1SoFar: Set<String>, path2SoFar: Set<String>): Int {
      val memoKey = MemoPart2Key(
        nodeKey,
        otherNodeKey,
        timeLeft,
        openedSoFar,
      )

      if (memoPart2.containsKey(memoKey)) {
        println("Cache hit $nodeKey $otherNodeKey $timeLeft")
        return memoPart2[memoKey]!!
      }

      if (timeLeft == 0) {
        return 0
      }

      var result = 0

      fun worthOpening(key: String): Boolean {
        return openedSoFar.contains(key).not() && nodes[key]!!.flow != 0
      }

      if (worthOpening(nodeKey) && worthOpening(otherNodeKey) && nodeKey != otherNodeKey) {
        result =
          max(
            (nodes[nodeKey]!!.flow + nodes[otherNodeKey]!!.flow) * (timeLeft - 1) + maxScorePart2(
              nodeKey,
              otherNodeKey,
              timeLeft - 1,
              openedSoFar + setOf(nodeKey, otherNodeKey),
              emptySet(),
              emptySet(),
            ),
            result
          )
      }
      if (worthOpening(nodeKey)) {
        nodes[otherNodeKey]!!.links.forEach { otherLink ->
          if (path2SoFar.contains(otherLink).not()) {
            result = max(
              result,
              nodes[nodeKey]!!.flow * (timeLeft - 1) + maxScorePart2(
                nodeKey,
                otherLink,
                timeLeft - 1,
                openedSoFar + setOf(nodeKey),
                emptySet(),
                path2SoFar + setOf(otherLink)
              )
            )
          }
        }
      }
      if (worthOpening(otherNodeKey)) {
        nodes[nodeKey]!!.links.forEach { link ->
          if (path1SoFar.contains(link).not()) {
            result = max(
              result,
              nodes[otherNodeKey]!!.flow * (timeLeft - 1) + maxScorePart2(
                link,
                otherNodeKey,
                timeLeft - 1,
                openedSoFar + setOf(otherNodeKey),
                path1SoFar + setOf(link),
                emptySet()
              )
            )
          }
        }
      }
      nodes[nodeKey]!!.links.forEach { link ->
        nodes[otherNodeKey]!!.links.forEach { otherLink ->
          result = max(result, maxScorePart2(link, otherLink, timeLeft - 1, openedSoFar,
            path1SoFar + setOf(link), path2SoFar + setOf(otherLink)
            ))
        }
      }


      memoPart2[memoKey] = result

      return result
    }

    data class MemoKey(
      val nodeKey: String,
      val timeLeft: Int,
      val openedSoFar: Set<String>,
    )

    private val memo2 = mutableMapOf<MemoKey, Int>()


    private var stepCount = 0
    fun maxScore2(nodeKey: String, timeLeft: Int, openedSoFar: Set<String>, pathSoFar: Set<String>): Int {
      val memoKey = MemoKey(
        nodeKey,
        timeLeft,
        openedSoFar,
      )

      if (memo2.containsKey(memoKey)) {
        return memo2[memoKey]!!
      }
      // 963946 iterations on real input
      // 861610 iterations with path so far optimisation
      println("Computing ${stepCount++} $nodeKey $timeLeft $openedSoFar $pathSoFar")

      if (timeLeft == 0) {
        return 0
      }

      var result = 0

      val node = nodes[nodeKey]!!

      if (openedSoFar.contains(nodeKey).not() && node.flow != 0) {
        result = max(
          result,
          (timeLeft - 1) * node.flow + maxScore2(nodeKey, timeLeft - 1, openedSoFar + setOf(nodeKey), emptySet())
        )
      }

      node.links.forEach { link ->
        if (pathSoFar.contains(link).not()) {
          result = max(result, maxScore2(link, timeLeft - 1, openedSoFar, pathSoFar + setOf(link)))
        }
      }
      memo2[memoKey] = result
      return result
    }

    fun maxScore(nodeKey: String, timeLeft: Int, openedSoFar: Set<String>, pathSoFar: Set<String>): Int {
      val memoKey = MemoKey(
        nodeKey,
        timeLeft,
        openedSoFar,
      )

      if (memo2.containsKey(memoKey)) {
        return memo2[memoKey]!!
      }

      // 1604907 iterations on real input
      println("Computing ${stepCount++} $nodeKey $timeLeft $openedSoFar $pathSoFar")

      val result = if (timeLeft <= 1) {
        0
      } else {
        val node = nodes[nodeKey]!!

        val nodeFlow = (timeLeft - 1) * node.flow

        val maxWithoutOpeningThis = node.links
          .filter { pathSoFar.contains(it).not() }
          .maxOfOrNull { maxScore2(it, timeLeft - 1, openedSoFar, pathSoFar + setOf(nodeKey)) }
          ?: if (openedSoFar.contains(nodeKey)) 0 else nodeFlow

        if (openedSoFar.contains(node.name) || node.flow == 0) {
          maxWithoutOpeningThis
        } else {
          val maxWithOpeningThis =
            node.links.map { maxScore2(it, timeLeft - 2, openedSoFar + listOf(nodeKey), emptySet()) }.max() + nodeFlow
          max(maxWithoutOpeningThis, maxWithOpeningThis)
        }
      }

      memo2[memoKey] = result
      return result
    }
  }

  fun part1() {
    val input = DataReader.read(16)
    val graph = GraphMap()
    input.forEach {
      graph.parseNode(it)
    }
    println(graph.maxScore2("AA", 30, emptySet(), emptySet()))
  }

  fun part2() {
    val input = DataReader.read(16)
    val graph = GraphMap()
    input.forEach {
      graph.parseNode(it)
    }
    println(graph.maxScore2("AA",  26, emptySet(), emptySet(), 1))
  }
}
