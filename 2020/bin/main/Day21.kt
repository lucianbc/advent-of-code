fun main() {
//    val a = mutableListOf(1, 2, 3)
//    println(a)
//    a.add(4)
//    val b = a.removeFirst()
//    println(a)
//    println(b)
    Day21.part2()
}

object Day21 {
    fun part1() {
        val recipes = read()
        
        val nonAlergens = findNonAlergens(recipes)
        
        val answer = recipes.fold(0) { acc, crt ->
            acc + crt.ingredients.intersect(nonAlergens).size
        }
        
        println(answer)
    }
    
    fun part2() {
        val recipes = read()
        val mappings = matchAlergens(recipes)
            .toSortedMap()
            .also { println(it) }
            .map { it.value }
        
        println(mappings.joinToString(","))
    }
    
    val regex = "((?:[a-z]* ?)+) \\(contains ((?:[a-z]*,? ?)+)\\)".toRegex()
    fun read() = DataReader.read(21)
        .map {
            val (first, second) = regex.find(it)!!.destructured
            val ingredients = first.split(" ").toSet()
            val alergens = second.split(", ").toSet()
            ingredients to alergens
            Recipe(ingredients, alergens)
        }
    
    
    data class Recipe(val ingredients: Set<String>, val alergens: Set<String>)
    
    fun intersectAlergens(recipes: List<Recipe>) = recipes
        .flatMap { r ->
            r.alergens.map { a -> a to r.ingredients }
        }
        .groupBy { it.first }
        .mapValues { it.value.map { t -> t.second }.reduce { a, b -> a.intersect(b) } }
    
    
    fun findNonAlergens(recipes: List<Recipe>): Set<String> {
        val allIngredients = recipes.fold(emptySet<String>()) { acc, crt -> acc.plus(crt.ingredients) }
    
        val maybeAlergens = intersectAlergens(recipes)
            .also { println(it) }
            .toList()
            .fold(emptySet<String>()) { acc, crt -> acc.plus(crt.second) }
            .also { println(it) }
        
        return allIngredients.minus(maybeAlergens)
    }
    
    fun matchAlergens(recipes: List<Recipe>): MutableMap<String, String> {
        val ambiguousAlergens = intersectAlergens(recipes)
            .toList()
            .toMutableList()
        
        var preciseMapping = ambiguousAlergens
            .find { it.second.size == 1 }
        
        val mappings = mutableMapOf<String, String>()
        
        while (preciseMapping != null) {
            val alergen = preciseMapping.second.first()
            mappings[preciseMapping.first] = alergen
            ambiguousAlergens.remove(preciseMapping)
            ambiguousAlergens.withIndex().forEach { (i, crt) ->
                if (crt.second.contains(alergen)) {
                    ambiguousAlergens[i] = crt.first to crt.second.minus(alergen)
                }
            }
            preciseMapping = ambiguousAlergens.find { it.second.size == 1 }
        }
        
        return mappings
    }
}