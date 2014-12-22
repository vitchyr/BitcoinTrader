import market.Market
import defs._
import scala.collection.mutable.Queue

package trader {
  // A trader that buys immediately if the current value is less than the mean
  // by some percentage. Sells if the current value is higher than the mean by
  // some percentage.
  class LowHighMeanTrader(
      val m: Market,
      var cash: Double,
      val currency: String,
      windowSize:Int,
      buyPercent: Double,
      sellPercent: Double)
    extends Trader {
    val HighThreshold: Double = 1 + sellPercent
    val LowThreshold: Double = 1 - buyPercent

    var bitcoins: Double = 0
    val pricesSeen: Queue[Double] = new Queue()
    var runningSum: Double = 0
    var nAveraged: Int = 0

    def update(): Unit = {
      val price = buyInfo.price
      runningSum += price
      pricesSeen.enqueue(price)
      if (nAveraged < windowSize) {
        nAveraged += 1
      } else {
        runningSum -= (pricesSeen.dequeue)
      }
    }
    def amountToSell: Double = {
      def isHigh =
        sellRate * windowSize > runningSum * HighThreshold
      // the first check makes sure that there's enough information to decide
      if (nAveraged == windowSize && isHigh && bitcoins != 0) {
        //println(s"Sell now! average = ${runningSum / windowSize}"+
        //  s". sell rate = $sellRate. bitcoins = $bitcoins")
        return bitcoins
      }
      0.0
    }
    def amountToBuy: Double = {
      def isLow = buyRate * windowSize < runningSum * LowThreshold
      if (nAveraged == windowSize && isLow && cash != 0) {
        //println(s"Buy now! average = ${runningSum / windowSize}"+
        //  s". buy rate = $buyRate. cash = $cash")
        return cash / buyRate
      }
      0.0
    }
  }

  class LowHighMeanTraderFactory(
      windowSize: Int,
      buyPercent: Double,
      sellPercent: Double)
    extends TraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): Trader =
      new LowHighMeanTrader(m, cash, currency, windowSize, buyPercent,
          sellPercent)

    override def toString = "Low-High Mean Trader"
  }
  object LowHighMeanTraderFactory {
    def apply(windowSize: Int, buyPercent: Double, sellPercent: Double) =
      new LowHighMeanTraderFactory(windowSize, buyPercent, sellPercent)
  }
}
