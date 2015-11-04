package schritt

 object ausgabe extends hans  {
  
  def ausgabe (a :Int ) : Boolean = {
    if ( a == 2)
    	false;
    else
    	true
  }

  def wasSollDas = {true}
  
  def main(args: Array[String]) = {
    Console.println( ausgabe(2) + "hans")
  }
}

trait hans {
  def wasSollDas : Boolean;
}

trait peter {
  def wasSollDas : Boolean;
}