import market.Market
import defs._

package trader {
  // A trader buys and sells bitcoins in an attempt to make money
  trait Trader { 
    // Tells the trader to try trading some money
    def trade(): Unit

    // Returns the amount of money left (capital not invested + money made)
    def moneyLeft: Double
  }

  trait TraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): Trader
  }
}
