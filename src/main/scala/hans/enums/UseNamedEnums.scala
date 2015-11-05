package enums


class UseNamedEnums {

//  import Planet._
//  def odel : PlanetVal = MERCURY
  
  import Status.{Value => StatusVal} 
  def odel2 : StatusVal = Status.aufrecht
  
  import Planet.{Value => PlanetVal}
  def odel1 : PlanetVal = Planet.MARS
  

}

object UseNamedEnums {
  
  def main(args: Array[String]) = println( new UseNamedEnums().odel2)
}