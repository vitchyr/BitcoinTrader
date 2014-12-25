import market.Market
import defs._

package trader {
  // A trader that buys immediately if he hasn't bought anything yet.
  // Only sells if he would make a profit.
  class StubbornTrader(val m: Market, var cash: Double, val currency: String)
      extends SingleTrader {
    var bitcoins: Double = 0
    private var moneyIfSold: Double = 0
    private var moneySpent: Double = 0

    def update(): Unit = {
      moneyIfSold = valueOf(bitcoins)
    }

    def amountToSell = {
      if (moneyIfSold > moneySpent) {
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

  object StubbornTraderFactory extends SingleTraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): SingleTrader =
      new StubbornTrader(m, cash, currency)

    override def toString = "Stubborn Trader Factory"
  }
}
