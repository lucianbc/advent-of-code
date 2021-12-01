import Day18.toExpression
import java.lang.Exception
import java.util.*

fun main() {
//    val expr = "5 + (8 * 3 + 9 + 3 * 4 * 3)"
    
    Day18.part2()
}

object Day18 {
    
    fun part1() {
        DataReader.read(18)
            .asSequence()
            .map { parseString(it) }
            .map { shuntingYard(it) }
            .map { evaluateExpression(it) }
            .also { println(it.toList()) }
            .sum()
            .also {
                println("Result it $it")
            }
    }
    
    fun part2() {
        DataReader.read(18)
            .asSequence()
            .map { parseString(it) }
            .map { shuntingYard(it, mapOf(Token.Star to 2, Token.Plus to 1)) }
            .map { evaluateExpression(it) }
            .also { println(it.toList()) }
            .sum()
            .also {
                println("Result it $it")
            }
    }
    
    fun evaluateExpression(tokens: Sequence<Token>): Long {
        val numbersStack = LinkedList<Long>()
        try {
            tokens.forEach {
                when (it) {
                    is Token.Number -> numbersStack.push(it.n.toLong())
                    Token.Plus -> {
                        val n1 = numbersStack.pop()
                        val n2 = numbersStack.pop()
                        numbersStack.push(n1 + n2)
                    }
                    Token.Star -> {
                        val n1 = numbersStack.pop()
                        val n2 = numbersStack.pop()
                        numbersStack.push(n1 * n2)
                    }
                    else -> error("Bad token in stream")
                }
            }
        } catch (e: Exception) {
            println("Error evaulating expression ${tokens.toExpression()}")
        }
        
        return numbersStack.first!!
    }
    
    fun shuntingYard(tokens: Sequence<Token>, precedence: Map<Token, Int> = mapOf(Token.Star to 1, Token.Plus to 1)): Sequence<Token> {
        val outputQueue: Queue<Token> = LinkedList()
        val operatorsStack = LinkedList<Token>()
        
        fun evaluatePrecedence(crt: Token, inStack: Token): Boolean {
            return precedence[crt]!! >= precedence[inStack]!!
        }
        
        tokens.forEach { token ->
            when (token) {
                is Token.Number -> outputQueue.add(token)
                Token.Plus, Token.Star -> {
                    while (operatorsStack.isNotEmpty() && !operatorsStack.last.isLeftParen()
                        && evaluatePrecedence(token, operatorsStack.last)) {
                        outputQueue.add(operatorsStack.last)
                        operatorsStack.removeLast()
                    }
                    operatorsStack.add(token)
                }
                Token.LeftParen -> operatorsStack.add(token)
                Token.RightParen -> {
                    while (operatorsStack.isNotEmpty() && !operatorsStack.last.isLeftParen()) {
                        outputQueue.add(operatorsStack.last)
                        operatorsStack.removeLast()
                    }
                    if (operatorsStack.isNotEmpty() && operatorsStack.last.isLeftParen()) {
                        operatorsStack.removeLast()
                    }
                }
            }
        }
        
        while (operatorsStack.isNotEmpty()) {
            outputQueue.add(operatorsStack.last)
            operatorsStack.removeLast()
        }
        
        return outputQueue.asSequence()
    }
    
    val parsingRegex = "(\\d+)|(\\+)|(\\*)|(\\()|(\\))".toRegex()
    
    fun parseString(expression: String): Sequence<Token> {
        return parsingRegex.findAll(expression).map {
            val (number, plus, star, leftParen, rightParen) = it.destructured
            when {
                number != "" -> {
                    Token.Number(number.toInt())
                }
                plus != "" -> {
                    Token.Plus
                }
                star != "" -> {
                    Token.Star
                }
                leftParen != "" -> {
                    Token.LeftParen
                }
                rightParen != "" -> {
                    Token.RightParen
                }
                else -> {
                    error("No match found")
                }
            }
        }
    }
    
    sealed class Token {
        data class Number(val n: Int): Token()
        object Plus : Token()
        object Star : Token()
        object LeftParen : Token()
        object RightParen : Token()
    
        fun isLeftParen(): Boolean {
            return when(this) {
                LeftParen -> true
                else -> false
            }
        }
    }
    
    
    fun Sequence<Token>.toExpression(): String {
        return this.map { when(it) {
            is Token.Number -> it.n.toString()
            Token.Plus -> "+"
            Token.Star -> "*"
            Token.LeftParen -> "("
            Token.RightParen -> ")"
        } }.joinToString(" ")
    }
}