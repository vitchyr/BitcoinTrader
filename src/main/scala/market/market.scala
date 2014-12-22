import defs._

package market { 
  trait Market {
    // Buy [amount] of bitcoints. Returns the amount sucessfully bought.
    def buy(amount: Double, currency: String): Transaction

    // Sell [amount] of bitcoints. Returns the amount sucessfully sold.
    def sell(amount: Double, currency: String): Transaction

    // Get the exchange rate. Returns how much 1 bitcoin costs in [currency]
    // The [Long] is the UTC timestamp of when this exchange rate is received.
    def getBuyInfo(currency: String): BitcoinInfo

    // How much a bitcoin can be sold for.
    def getSellInfo(currency: String): BitcoinInfo

    // Update the information about this market.
    def update(): Unit

    def sellCut: Double // = cost to sell / cost to buy 1 BTC
  }
}
