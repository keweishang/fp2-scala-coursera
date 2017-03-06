package week3

/**
  * Created by kshang on 03/03/2017.
  */
abstract class Simulation {

  // Action is a function with no parameter and returns no parameter
  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private var curtime = 0
  def currentTime: Int = curtime

  private var agenda: List[Event] = List()

  private def insert(ag: List[Event], item: Event) : List[Event] = ag match {
    case first :: rest if first.time <= item.time => first :: insert(rest, item)
    case _ => item :: ag
  }

  // block is an expression that returns nothing
  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  def run(): Unit = {
    afterDelay(0) {
      println("*** simulation started, time = " + currentTime + " ***")
    }
    loop()
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }

}
