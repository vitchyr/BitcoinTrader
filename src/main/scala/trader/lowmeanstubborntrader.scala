import market.Market
import defs._
import scala.collection.mutable.Queue

package trader {
  // A trader that buys immediately if the current value is less than the mean
  // by some percentage. Sells only if he would make a profit.
  class LowMeanStubbornTrader(
      val m: Market,
      var cash: Double,
      val currency: String,
      windowSize:Int,
      buyPercent: Double)
    extends SingleTrader {
    var bitcoins: Double = 0

    private val LowThreshold: Double = 1 - buyPercent

    // Low mean logic
    private val pricesSeen: Queue[Double] = new Queue()
    private var runningSum: Double = 0
    private var nAveraged: Int = 0

    // Stubborn Logic
    private var moneyIfSold: Double = 0
    private var moneySpent: Double = 0

    def update(): Unit = {
      moneyIfSold = valueOf(bitcoins)

      val price = btcPrice
      runningSum += price
      pricesSeen.enqueue(price)
      if (nAveraged < windowSize) {
        nAveraged += 1
      } else {
        runningSum -= (pricesSeen.dequeue)
      }
    }

    def amountToSell = {
      if (moneyIfSold > moneySpent) {
        bitcoins
      } else {
        0.0
      }
    }

    def amountToBuy: Double = {
      def isLow = btcPrice * windowSize < runningSum * LowThreshold
      if (nAveraged == windowSize && cash != 0 && isLow) {
        return maxBTCsCanBuy
      }
      0.0
    }

    def updateAfterSell(trans: Transaction): Unit = moneySpent = 0

    def updateAfterBuy(trans: Transaction): Unit = moneySpent = -trans.dCash

    def name = "Low Mean Stubborn Trader"
  }

  class LowMeanStubbornTraderFactory(
      windowSize: Int,
      buyPercent: Double)
    extends SingleTraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): SingleTrader =
      new LowMeanStubbornTrader(m, cash, currency, windowSize, buyPercent)

    override def toString = "Low Mean Stubborn Trader Factory"
  }
  object LowMeanStubbornTraderFactory {
    def apply(windowSize: Int, buyPercent: Double) =
      new LowMeanStubbornTraderFactory(windowSize, buyPercent)
  }
}
