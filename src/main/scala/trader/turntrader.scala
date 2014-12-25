import market.Market
import defs._
import scala.collection.mutable.Queue

package trader {
  /* A trader who buys when the price starts to rise, and sells when the price
   * starts to drop. */
  class TurnTrader(
      val m: Market,
      var cash: Double,
      val currency: String,
      windowSize: Int) // How many previous points to base the turn off of.
    extends SingleTrader {
    var bitcoins: Double = 0

    private val nData: Int = 0

    private val Up = 1
    private val Down = 0

    def update(): Unit = ()

    private def wasGoing(direction: Int) = direction match {
       case Up => false
       case Down => false
       case _ =>
         sys.error(s"TurnTrader.wasGoing: invalid direction: $direction")
    }

    private def turned(direction: Int) = direction match {
       case Up => false
       case Down => false
       case _ => sys.error(s"TurnTrader.turned: invalid direction: $direction")
    }

    def amountToSell: Double = {
      if (nData > windowSize && wasGoing(Up) && turned(Down)) {
        return bitcoins
      }
      0.0
    }

    def amountToBuy: Double = {
      if (nData > windowSize && wasGoing(Down) && turned(Up)) {
        return maxBTCsCanBuy
      }
      0.0
    }

    def updateAfterSell(trans: Transaction): Unit = ()

    def updateAfterBuy(trans: Transaction): Unit = ()

    def name = "Low-High Mean Trader"
  }

  class TurnTraderFactory(windowSize: Int) extends SingleTraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): SingleTrader =
      new TurnTrader(m, cash, currency, windowSize)

    override def toString = "Low-High Mean Trader Factory"
  }

  object TurnTraderFactory {
    def apply(windowSize: Int) = new TurnTraderFactory(windowSize)
  }
}
