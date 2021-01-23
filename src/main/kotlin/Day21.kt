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
    enum class TheoremResponse {
        Proven,
        False,
        Undecidable
    }
    
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
    
    /*
    Treat recipes like evidences. A theorem is something like "ingredient x is alergen y".
    To prove this theorem, I first filter all the recipes to find the ones containing that ingredient.
    Then:
      - the theorem is true if it satisfies all the recipes AND there is at least one recipe where the
            only alergen is the one I am proving and that rule does not contain any other ingredient so far
      - the theorem is false if there is one recipe that contains the ingredient, but doesn't contain the alergen
      - by default it is undecidable
     */
    fun prove(ingredient: String, alergen: String, recipes: List<Recipe>): TheoremResponse {
        
        val initialReduceValue = Triple(
            emptyList<Recipe>(), // recipes that contain the ingredient and multiple alergens
            emptyList<Recipe>(), // recipes tht contain the ingredient and a single alergen
            emptyList<Recipe>(), // recipes that contain the ingredient, but not the alergen or the alergen, but not the ingredient
        )
        
        val (_, singleAlergen, contradictions) = recipes.fold(initialReduceValue) { acc, crt ->
            val (multiAlergens, singleAlergen, contradictions) = acc
            
            val crtHasAlergen = crt.alergens.contains(alergen)
            val crtHasIngredient = crt.ingredients.contains(ingredient)
            
            if (crtHasAlergen && crtHasIngredient) {
                if (crt.alergens.size == 1) {
                    Triple(multiAlergens, singleAlergen.plus(crt), contradictions)
                } else {
                    Triple(multiAlergens.plus(crt), singleAlergen, contradictions)
                }
            } else if (!crtHasAlergen && !crtHasIngredient) {
                acc
            } else {
                Triple(multiAlergens, singleAlergen, contradictions.plus(crt))
            }
        }
        
        return when {
            contradictions.isNotEmpty() -> {
                TheoremResponse.False
            }
            singleAlergen.isNotEmpty() -> {
                TheoremResponse.Proven
            }
            else -> {
                TheoremResponse.Undecidable
            }
        }
    }
}