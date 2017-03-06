package week3

/**
  * Created by kshang on 05/03/2017.
  */
abstract class Gates extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {

    private var sigVal = false
    private var actions : List[Action] = List()

    def getSignal = sigVal

    def setSignal(s: Boolean) =
      if (s != sigVal) {
        sigVal = s
        // call each action without parameter
        actions foreach(_())
      }

    def addAction(a: Action) = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        // pass the block, which is an expression
        output setSignal !inputSig
      }
    }
    input addAction invertAction
  }

  def andGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction() = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(AndGateDelay) {
        // pass the block, which is an expression
        output setSignal (in1Sig & in2Sig)
      }
    }
    in1 addAction andAction
    in2 addAction andAction
  }

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction() = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) {
        // pass the block, which is an expression
        output setSignal (in1Sig | in2Sig)
      }
    }
    in1 addAction orAction
    in2 addAction orAction
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(name + " " + currentTime + " new-value = " + wire.getSignal)
    }
    wire addAction probeAction
  }
}
