package meap


trait Monoid[T] {
    def zero: T
    def op(t1: T, t2: T): T
}

sealed trait Currency 
case object USD extends Currency
case object JPY extends Currency
case object AUD extends Currency
case object INR extends Currency
case class Money(m: Map[Currency, BigDecimal])

object odel {
//    import meap.Monoid
//    final val zeroMoney: Money = Money(Monoid[Map[Currency, BigDecimal]].zero)
//   // final val zeroMoney: Money = Money(Map[Currency, BigDecimal])
//
//    implicit def MoneyAdditionMonoid = new Monoid[Money] {
//        val m = implicitly[Monoid[Map[Currency, BigDecimal]]]
//        def zero = zeroMoney 
//        def op(m1: Money, m2: Money) = Money(m.op(m1.m, m2.m))
//}
}