import defs._

package market { 
  trait Market {
    // Sell [amount] of bitcoints. Returns the amount sucessfully sold.
    def sell(amount: Double, currency: String): Transaction

    // Buy [amount] of bitcoints. Returns the amount sucessfully bought.
    def buy(amount: Double, currency: String): Transaction

    // Get a predicted transaction if you were to sell [amount] BTCs
    def quoteToSell(amount: Double, currency: String): Transaction

    // Get a predicted transaction if you were to buy [amount] BTCs
    def quoteToBuy(amount: Double, currency: String): Transaction

    // Get a predicted transaction if you were to sell [amount] worth of BTC
    def quoteToSellCash(amount: Double, currency: String): Transaction

    // Get a predicted transaction if you were to buy [amount] worth of BTC
    def quoteToBuyCash(amount: Double, currency: String): Transaction

    // Update the information about this market.
    def update(): Unit

    // Returns true iff the market is still . If not, do not call any other
    // method besides open()
    def isOpen(): Boolean

    // Open up the market for the first time (i.e. initialize things.)
    def open(): Unit
  }
}
