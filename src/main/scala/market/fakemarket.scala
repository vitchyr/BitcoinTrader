import defs._

package market { 
  // Fake markets use fake data for the buy/sell prices.
  trait FakeMarket extends Market with Iterable[Double] {
    val SellCut: Double = .98
    var lastBuyRate: Option[Double] = None

    // in milliseconds
    def time(): Long = System.currentTimeMillis

    def getLastBuyRate(): Double = lastBuyRate match {
      case Some(rate) => rate
      case None => {
        val newRate = this.iterator.next()
        lastBuyRate = Some(newRate)
        newRate
      }
    }

    def sell(amount: Double, currency: String): Transaction =
      if (amount <= 0) sys.error(s"Cannot sell $amount BTCs") else
      new Transaction(-amount, SellCut * amount * getLastBuyRate(), time, 
          currency)

    def buy(amount: Double, currency: String): Transaction =
      if (amount <= 0) sys.error(s"Cannot buy $amount BTCs") else
      new Transaction(amount, -amount * getLastBuyRate(), time, currency)

    def quoteToSell(amount: Double, currency: String): Transaction =
      new Transaction(-amount, SellCut * amount * getLastBuyRate(), time, 
          currency)

    def quoteToBuy(amount: Double, currency: String): Transaction =
      new Transaction(amount, -amount * getLastBuyRate(), time, currency)

    def quoteToSellCash(amount: Double, currency: String): Transaction =
      quoteToSell(amount / SellCut / getLastBuyRate(), currency)

    def quoteToBuyCash(amount: Double, currency: String): Transaction =
      quoteToBuy(amount / getLastBuyRate(), currency)

    def update(): Unit =
      lastBuyRate = Some(this.iterator.next())
  }
}
