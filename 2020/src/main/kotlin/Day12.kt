import Day12.Orientation.*
import Day12.TurnCommand.*
import java.lang.Exception
import kotlin.math.absoluteValue
import kotlin.math.cos
import kotlin.math.sin

fun main() {
    Day12.part1()
}

private object Day12 {
    enum class Orientation {
        North,
        South,
        East,
        West
    }
    
    sealed class TurnCommand {
        data class Left(val degrees: Int): TurnCommand()
        data class Right(val degrees: Int): TurnCommand()
    }
    
    data class Position(val east: Int, val north: Int)
    
    data class State(val position: Position, val orientation: Orientation)
    
    fun part1() {
        val pos = DataReader.read(12).fold(State(Position(0, 0), East)) { acc, line ->
            handleCommand(line, acc)
        }
        
        println(pos.position)
        println(pos.position.east.absoluteValue + pos.position.north.absoluteValue)
    }
    
    fun handleCommand(command: String, state: State): State {
        val cmd = command.substring(0, 1)
        val nr = command.substring(1).toInt()
    
        when (cmd) {
            "L" -> {
                val newOrientation = computeTurn(state.orientation, Left(nr))
                return state.copy(orientation = newOrientation)
            }
            "R" -> {
                val newOrientation = computeTurn(state.orientation, Right(nr))
                return state.copy(orientation = newOrientation)
            }
            "F" -> {
                val newPosition = when (state.orientation) {
                    North -> state.position.copy(north = state.position.north + nr)
                    South -> state.position.copy(north = state.position.north - nr)
                    East -> state.position.copy(east = state.position.east + nr)
                    West -> state.position.copy(east = state.position.east - nr)
                }
                return state.copy(position = newPosition)
            }
            "W" -> {
                val newPosition = state.position.copy(east = state.position.east - nr)
                return state.copy(position = newPosition)
            }
            "E" -> {
                val newPosition = state.position.copy(east = state.position.east + nr)
                return state.copy(position = newPosition)
            }
            "S" -> {
                val newPosition = state.position.copy(north = state.position.north - nr)
                return state.copy(position = newPosition)
            }
            "N" -> {
                val newPosition = state.position.copy(north = state.position.north + nr)
                return state.copy(position = newPosition)
            }
            else -> {
                throw Exception("Unknown command $command")
            }
        }
    }
    
    fun computeTurn(current: Orientation, command: TurnCommand): Orientation {
        val (sinA, cosA) = current.toPoint()
        val b = when(command) {
            is Left -> command.degrees
            is Right -> -command.degrees
        }
        val sinB = sinD(b.toDouble())
        val cosB = cosD(b.toDouble())
        
        val sinAB = (sinA * cosB + cosA * sinB).toInt()
        val cosAB = (cosA * cosB - sinA * sinB).toInt()
        
        return (sinAB to cosAB).toOrientation()
    }
    
    fun Orientation.toPoint() = when(this) {
        North -> 1 to 0
        South -> -1 to 0
        East -> 0 to 1
        West -> 0 to -1
    }
    
    fun Pair<Int, Int>.toOrientation() =
        when (this) {
            1 to 0 -> North
            -1 to 0 -> South
            0 to 1 -> East
            0 to -1 -> West
            else -> throw Exception("Point $this is not an orientation")
        }
    
    fun sinD(x: Double) = sin(x * Math.PI / 180)
    fun cosD(x: Double) = cos(x * Math.PI / 180)
}

private object tests {
    fun test1() {
        val crts = listOf(North, South, East, West)
        val orients = listOf(Left(90), Left(180), Left(270), Right(90), Right(180), Right(270))
        
        val results = crts
            .flatMap { orients.map { it2 -> it to it2 } }
            .map { Day12.computeTurn(it.first, it.second) }
            
        println(results)
    }
}