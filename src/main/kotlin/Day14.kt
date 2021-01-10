fun main() {
    Day14.part2()
}

object Day14 {
    val memRegex = "mem\\[(\\d*)\\] = (\\d*)".toRegex()
    fun part1() {
        DataReader.read(14)
            .forEach {
                if (it.startsWith("mask = ")) {
                    Memory.setMast(it.substring(7))
                }
                else {
                    val (addr, value) = memRegex.find(it)!!.destructured
                    Memory.setValue(addr.toInt(), value.toLong())
                }
            }
        println(Memory.getSumOfAll())
    }
    
    fun part2() {
        DataReader.read(14)
            .forEach {
                if (it.startsWith("mask = ")) {
                    Memory.setMast(it.substring(7))
                }
                else {
                    val (addr, value) = memRegex.find(it)!!.destructured
                    Memory.setValueV2(addr.toInt(), value.toLong())
                }
            }
        println(Memory.getSumOfAll())
    }
    
    object Memory {
        private var mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        
        private val values = mutableMapOf<Long, Long>()
        
        // Part 1
        fun setValue(addr: Int, value: Long) {
            value
                .let(::transformToBinary)
                .let(::applyMask)
                .let(::transformFromBinary)
                .also {
                    values[addr.toLong()] = it
                }
        }
        
        // Part 2
        fun setValueV2(addr: Int, value: Long) {
            addr
                .let { transformToBinary(it.toLong()) }
                .let(::applyMaskV2)
                .let(::expandFloating)
                .onEach { r ->
                    values[transformFromBinary(r)] = value
                }
        }
        
        fun applyMaskV2(repr: String) = repr.zip(mask)
            .map {
                when (it.second) {
                    '0' -> it.first
                    '1' -> '1'
                    else -> 'X'
                }
            }
            .joinToString("")
        
        fun expandFloating(repr: String): List<String> {
            return repr.fold(listOf("")) { acc, crt ->
                if (crt != 'X')
                    acc.map { "$it$crt" }
                else
                    acc.flatMap { listOf("${it}0", "${it}1") }
            }
        }
        
        // Uttilities
        fun transformToBinary(number: Long): String {
            val size = 36
            fun recursiveTransform(current: Long, repr: String = ""): String {
                if (current == 0L) {
                    if (repr.length < size) {
                        return "0".repeat(size - repr.length) + repr
                    }
                    return repr
                } else {
                    return recursiveTransform(current / 2, "${current % 2}$repr")
                }
            }
            return recursiveTransform(number)
        }
        
        fun transformFromBinary(repr: String): Long {
            return repr.reversed()
                // Made a mistake here because I haven't initially set 1 to be long
                // So after 2^31 the 2 power was wrapping
                .fold(0L to 1L) { acc, crt ->
                    if (crt == '0') acc.first to acc.second * 2
                    else acc.first + acc.second to acc.second * 2
                }.first
        }
    
        fun applyMask(repr: String): String {
            return repr.zip(mask)
                .map { if (it.second == 'X') it.first else it.second }
                .joinToString("")
        }
        
        fun setMast(mask: String) {
            this.mask = mask
        }
        
        fun getSumOfAll() = values.values.sum()
    }
}