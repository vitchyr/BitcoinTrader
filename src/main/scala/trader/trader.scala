import market.Market
import defs._

package trader {
  // A trader buys and sells bitcoins in an attempt to make money
  trait Trader { 
    val m: Market
    var nTT: Int = 0 // how many times was "trade()" actualy called?

    // Tells the trader to try trading some money
    def trade(): Unit

    def tryToTrade(): Unit = {
      if (m.isOpen) {
        nTT += 1
        trade()
      }
    }

    def nTradesTried = nTT

    // Returns the amount of money left (capital not invested + money made)
    def moneyLeft: Double
  }

  trait TraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): Trader
  }
}
