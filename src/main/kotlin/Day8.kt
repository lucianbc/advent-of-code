fun main() = part2()

fun<T> List<T>.modify(index: Int, value: T): List<T> {
    return this.subList(0, index) + listOf(value) + this.subList(index + 1, this.size)
}

private fun part1() {
    val data = DataReader.read(8).map(::parse)
    val acc = simulate(data).acc
    
    println("Acc is $acc")
}

private fun part2() {
    val data = DataReader.read(8).map(::parse)
    fun Instruction.isFlippable(): Boolean = when (this) {
        is Instruction.Nop -> true
        is Instruction.Acc -> false
        is Instruction.Jmp -> true
    }
    
    fun Instruction.flip(): Instruction = when (this) {
        is Instruction.Nop -> Instruction.Jmp(this.offset)
        is Instruction.Acc -> this
        is Instruction.Jmp -> Instruction.Nop(this.offset)
    }
    
    val potentials = data.asSequence().withIndex()
        .filter { it.value.isFlippable() }
        .map { data.modify(it.index, it.value.flip()) }
        .map {
            simulate(it)
        }
        .filter { when(it) {
            is Result.Loop -> false
            is Result.Terminate -> true
        } }
    
    println(potentials.toList())
}

fun parse(line: String): Instruction {
    val instr = line.substring(0, 3)
    val offset = line.substring(4).toInt()
    return when (instr) {
        "nop" -> Instruction.Nop(offset)
        "acc" -> Instruction.Acc(offset)
        else -> Instruction.Jmp(offset)
    }
}

fun simulate(instructions: List<Instruction>): Result {
    val visited = BooleanArray(instructions.size) { false }
    var acc = 0
    var linePointer = 0
    while (true) {
        if (linePointer >= instructions.size) {
            return Result.Terminate(acc)
        }
        val line = instructions[linePointer]
        if (visited[linePointer]) {
            return Result.Loop(acc)
        } else {
            visited[linePointer] = true
        }
        
        when (line) {
            is Instruction.Nop -> linePointer++
            is Instruction.Acc -> {
                acc += line.value
                linePointer++
            }
            is Instruction.Jmp -> {
                linePointer += line.offset
            }
        }
    }
}

sealed class Instruction {
    data class Nop(val offset: Int): Instruction()
    data class Acc(val value: Int): Instruction()
    data class Jmp(val offset: Int): Instruction()
}

sealed class Result(open val acc: Int) {
    data class Loop(override val acc: Int) : Result(acc)
    data class Terminate(override val acc: Int) : Result(acc)
}
