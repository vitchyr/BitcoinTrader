import market.Market
import defs._

package trader {
  // A trader that buys immediately if he hasn't bought anything yet.
  // Only sells if he would make a profit.
  class StubbornTrader(
      val m: Market,
      var cash: Double,
      var bitcoins: Double,
      val currency: String,
      sellPercent: Double)
    extends SingleTrader {
    private val HighThreshold: Double = 1 + sellPercent
    private var moneyIfSold: Double = 0
    private var moneySpent: Double = 0

    def update(): Unit = {
      moneyIfSold = valueOf(bitcoins)
    }

    def amountToSell = {
      if (moneyIfSold > HighThreshold * moneySpent) {
        bitcoins
      } else {
        0.0
      }
    }

    def amountToBuy = {
      if (moneySpent == 0) {
        maxBTCsCanBuy
      } else {
        0.0
      }
    }

    def updateAfterSell(trans: Transaction): Unit = moneySpent = 0

    def updateAfterBuy(trans: Transaction): Unit = moneySpent = -trans.dCash

    def name = "Stubborn Trader"
  }

  class StubbornTraderFactory(sellPercent: Double)
      extends SingleTraderFactory {
    def newTrader(
        m: Market,
        cash: Double,
        btc: Double,
        currency: String): SingleTrader =
      new StubbornTrader(m, cash, btc, currency, sellPercent)

    override def toString = "Stubborn Trader Factory"
  }
  object StubbornTraderFactory {
    def apply(sellPercent: Double) =
      new StubbornTraderFactory(sellPercent) 
  }
}
