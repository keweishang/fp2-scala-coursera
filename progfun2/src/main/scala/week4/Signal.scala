package week4

/**
  * Created by kshang on 10/03/2017.
  */
class Signal[T](expr: => T) {

  import Signal._
  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())
    // a signal's current value can change when the value of a dependent signal changes
    if (myValue != newValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }

  // to use (dereference) a signal
  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }
}

object Signal {
  val caller = new StackableVariable[Signal[_]](NoSignal)
  // to create a signal
  def apply[T](expr: => T) = new Signal(expr)
}

// we also evaluate signal expressions at the top-level when there is no other signal that's defined or updated
// we use the sentinel object NoSignal as the caller for these expressions
object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}