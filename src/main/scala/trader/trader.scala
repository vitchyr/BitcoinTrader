import market.Market
import defs._

package trader {
  // A trader buys and sells bitcoins in an attempt to make money
  trait Trader { 
    // how much money this trader started off with.
    protected val initialCash: Double

    // how much cash/bitcoins the trader has now.
    def getCash: Double
    def getBtc: Double
    def setCash(c: Double): Unit
    def setBtc(b: Double): Unit

    val m: Market // Where the trader trades
    val currency: String // What currency the trader trades in.

    /* How many times did the trader go to the market?
     * i.e. # times trade() was called */
    var nTradesTried: Int = 0

    def nSells: Int // how many times the trader sold BTCs
    def nBuys: Int // how many times the trader bought BTCs

    /* Tell the trader to try to go to the market and trade. May not actually
     * trade because the market (e.g.) may be closed. */
    def tryToTrade(): Unit = {
      if (m.isOpen) {
        nTradesTried += 1
        trade()
      }
    }

    // Tells the trader to try trading some money. Assumes the market is open.
    def trade(): Unit

    // A log of past transactions
    def history: TraderHistory

    /* Returns the amount of money left if the trader were to cash out now (or
     * right when the market has closed) */
    def moneyLeft: Double

    /* Returns the percentage that the captial changed. */
    def returns: Double = moneyLeft / initialCash * 100.0

    /* The name of this type of trader. Don't override toString because the
     * default toString is nice for debugging. */
    def name: String

    // Amount of cash/bitcoins lost due to rounding
    def cashLostToRounding: Double
    def btcLostToRounding: Double
  }

  trait TraderFactory {
    def newTrader(
        m: Market,
        cash: Double,
        btc: Double,
        currency: String): Trader
  }
}
