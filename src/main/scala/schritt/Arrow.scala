package schritt

abstract class Food(val name: String) {
    override def toString = name
}

class Recipe( val name: String, val ingredients: List[Food], val instructions: String) {
    override def toString = name
}

object Apple extends Food("Apple")
object Orange extends Food("Orange")
object Cream extends Food("Cream")
object Sugar extends Food("Sugar")

trait SimpleFoods {
    object Pear extends Food("Pear")
    def allFoods = List(Apple, Pear)
    def allCategories = Nil
}
object FruitSalad extends Recipe( "fruit salad", List(Apple, Orange, Cream, Sugar), "Stir it all together.")

trait FoodCategories {
    case class FoodCategory(name: String, foods: List[Food])
    def allCategories: List[FoodCategory]
}

abstract class Database extends FoodCategories {
    def allFoods: List[Food]
    def allRecipes: List[Recipe]
    def foodNamed(name: String) = allFoods.find(f => f.name == name)
}

trait SimpleRecipes { // Does not compile
    this: SimpleFoods =>
    object FruitSalad extends Recipe( "fruit salad", List(Apple, Pear), "Mix it all together.")
    def allRecipes = List(FruitSalad)
}




