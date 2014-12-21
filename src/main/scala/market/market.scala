package market { 
  // Holds how many bitcoins changes for how much cash in [currency]
  class Transaction(val dBitcoin: Double, val dCash: Double,
      val currency: String)

  trait Market {
    // Buy [amount] of bitcoints. Returns the amount sucessfully bought.
    def buy(amount: Double, currency: String): Transaction

    // Sell [amount] of bitcoints. Returns the amount sucessfully sold.
    def sell(amount: Double, currency: String): Transaction

    // Get the exchange rate. Returns how much 1 bitcoin costs in [currency]
    // The [Long] is the UTC timestamp of when this exchange rate is received.
    def getBuyInfo(currency: String): Tuple2[Double, Long]

    // How much a bitcoin can be sold for.
    def getSellInfo(currency: String): Tuple2[Double, Long]
  }
}
