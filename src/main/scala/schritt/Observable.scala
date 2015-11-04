package schritt

trait Observable {
    
    type Handle
    
    var callbacks : Map[Handle,this.type => Unit] = Map.empty
    
    def observe(callback: this.type => Unit): Handle = {
        val handle = createHandle(callback)
        callbacks += (handle -> callback)
        handle
    }
    def unobserve(handle: Handle) : Unit = {
        callbacks -= handle
    }

    protected def createHandle(callback: this.type => Unit): Handle
    protected def notifyListeners() : Unit = for(callback <- callbacks.values) callback(this)
}

trait DefaultHandles extends Observable {
    type Handle = (this.type => Unit)
    protected def createHandle(callback: this.type => Unit): Handle = callback
}

class IntStore(private var value: Int) extends Observable with DefaultHandles {
    def get : Int = value
    def set(newValue : Int) : Unit = {
        value = newValue
        notifyListeners()
    }
    override def toString : String = "IntStore(" + value + ")"
}

object runObservable {
  def main(args: Array[String]) : Unit = {

    val callback = println(_ : Any)
    
    val x = new IntStore(5)
    val handle = x.observe(callback)
    x.set(2)
    x.unobserve(handle)
    x.set(4)
    
   val y = new IntStore(2)
   val handle1 = y.observe(callback)
   
   println( handle1 == handle)
   
   y.unobserve(handle1)
  }
}
