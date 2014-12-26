import market.Market
import defs._
import scala.collection.mutable.Queue
import mathutil.Regression

package trader {
  /* A trader who buys when the price starts to rise, and sells when the price
   * starts to drop.
   *
   * To tell if a price is rising/dropping, do a linear fir and see what the
   * slope is. If it's above a min, it's rising. If it's below a max, it's
   * dropping. */
  class TurnTrader(
      val m: Market,
      var cash: Double,
      val currency: String,
      windowSize: Int, // How many previous points to base the turn off of.
      minRisingSlope: Double, // Min slope for price to be rising
      maxDroppingSlope: Double, // Max slope for price to be dropping
      minTurnChange: Double) // The min change in slope back towards zero to
                             // consider that there's a change in direction.
    extends SingleTrader {
    var bitcoins: Double = 0

    private val pricesSeen: Queue[Double] = new Queue()
    private var nData: Int = 0
    private var slope: Double = 0
    private var lastSlope: Double = 0

    // experimental stuff
    private var price: Double = 0
    private var lastPrice: Double = 0
    private var boughtSomething: Boolean = false

    private val Up = 1
    private val Down = 0

    def update(): Unit = {
      val newPrice = btcPrice
      pricesSeen.enqueue(newPrice)
      nData += 1
      if (nData >= windowSize) {
        pricesSeen.dequeue
        val (_, b) = Regression.linSpaceFit(pricesSeen.toList)
        if (nData > windowSize) {
          lastSlope = slope
          lastPrice = price
        }
        slope = b
        price = price
        println(s"Time ${nData - 1}: $slope")
      } else {
        lastPrice = newPrice
      }
    }

    private def wasGoing(direction: Int) = direction match {
       case Up => slope > minRisingSlope
       case Down => slope < maxDroppingSlope
       case _ =>
         sys.error(s"TurnTrader.wasGoing: invalid direction: $direction")
    }

    private def turned(direction: Int) = direction match {
       case Up => slope - lastSlope > minTurnChange || slope > 0
       case Down => lastSlope - slope > minTurnChange || slope < 0
       case _ => sys.error(s"TurnTrader.turned: invalid direction: $direction")
    }

    def amountToSell: Double = {
      if (nData > windowSize && wasGoing(Up) && turned(Down)) {
        println(s"SELL $bitcoins BTCs! Time is $nData")
        return bitcoins
      }
      0.0
    }

    def amountToBuy: Double = {
      if (nData > windowSize && wasGoing(Down) && turned(Up)) {
        println(s"BUY $maxBTCsCanBuy BTCs! Time is $nData")
        return maxBTCsCanBuy
      }
      /*
      if (nData > windowSize && !boughtSomething) { // tired of waiting
        println(s"Let's start! But $maxBTCsCanBuy BTCs. Time is $nData")
        return maxBTCsCanBuy
      }
      */
      0.0
    }

    def updateAfterSell(trans: Transaction): Unit = ()

    def updateAfterBuy(trans: Transaction): Unit = boughtSomething = true

    def name = "Turn Trader"
  }

  class TurnTraderFactory(
      windowSize: Int,
      minRisingSlope: Double,
      maxDroppingSlope: Double,
      minTurnChange: Double)
    extends SingleTraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): SingleTrader =
      new TurnTrader(
        m,
        cash,
        currency,
        windowSize,
        minRisingSlope,
        maxDroppingSlope,
        minTurnChange)

    override def toString = "Turn Trader Factory"
  }

  object TurnTraderFactory {
    def apply(
        windowSize: Int,
        minRisingSlope: Double,
        maxDroppingSlope: Double,
        minTurnChange: Double) =
      new TurnTraderFactory(
        windowSize,
        minRisingSlope,
        maxDroppingSlope,
        minTurnChange)
  }
}
