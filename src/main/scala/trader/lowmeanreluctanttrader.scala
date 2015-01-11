import market.Market
import defs._
import scala.collection.mutable.Queue

package trader {
  // A trader that buys immediately if the current value is less than the mean
  // by some percentage. Sells only if he would make a profit.
  class LowMeanReluctantTrader(
      val m: Market,
      var cash: Double,
      var bitcoins: Double,
      val currency: String,
      maxNumUpdates: Int,
      windowSize:Int,
      buyPercent: Double)
    extends SingleTrader {

    private val LowThreshold: Double = 1 - buyPercent

    // members for low mean logic
    private val pricesSeen: Queue[Double] = new Queue()
    private var runningSum: Double = 0
    private var nAveraged: Int = 0

    // members for reluctant logic
    private var moneyIfSold: Double = 0
    private var moneySpent: Double = 0
    private var idxBought: Int = 0 // = nUpdates when last purchase was made
    private var nUpdates: Int = 0 // number of times update was called

    def update(): Unit = {
      val sellQ = sellQuote(bitcoins)
      moneyIfSold = sellQ.dCash
      nUpdates += 1

      val price = btcPrice
      runningSum += price
      pricesSeen.enqueue(price)
      if (nAveraged < windowSize) {
        nAveraged += 1
      } else {
        runningSum -= (pricesSeen.dequeue)
      }
    }

    def amountToSell: Double = {
      if (bitcoins != 0 &&
          ((nUpdates - idxBought) > maxNumUpdates
          || moneyIfSold > moneySpent)) {
        return bitcoins
      }
      0.0
    }

    def amountToBuy: Double = {
      def isLow = btcPrice * windowSize < runningSum * LowThreshold
      if (nAveraged == windowSize && cash != 0 && isLow) {
        return maxBTCsCanBuy
      }
      0.0
    }

    def updateAfterSell(trans: Transaction): Unit = {
      moneySpent = 0
      idxBought = 0
    }

    def updateAfterBuy(trans: Transaction): Unit = {
      moneySpent = -trans.dCash
      idxBought = nUpdates
    }

    def name = "Low Mean Reluctant Trader"
  }

  class LowMeanReluctantTraderFactory(
      maxNumUpdates: Int,
      windowSize: Int,
      buyPercent: Double)
    extends SingleTraderFactory {
    def newTrader(
        m: Market,
        cash: Double,
        btc: Double,
        currency: String): SingleTrader =
      new LowMeanReluctantTrader(m, cash, btc, currency, maxNumUpdates,
          windowSize, buyPercent)

    override def toString = "Low Mean Reluctant Trader Factory"
  }
  object LowMeanReluctantTraderFactory {
    def apply(maxNumUpdates: Int, windowSize: Int, buyPercent: Double) =
      new LowMeanReluctantTraderFactory(maxNumUpdates, windowSize, buyPercent)
  }
}
