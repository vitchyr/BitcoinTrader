import defs._

package market { 
  // Fake markets use fake data for the buy/sell prices.
  trait FakeMarket extends Market with Iterable[Double] {
    val SellCut: Double = .99
    var savedBuyRate: Option[Double] = None

    // Reset the state of the class as if it had just been initialized
    def resetState(): Unit

    // Reset the fake market as if we had never used it before
    def reset(): Unit = {
      resetState()
      savedBuyRate = None
      open()
    }

    // in milliseconds
    def time(): Long = System.currentTimeMillis

    private def lastBuyRate: Double = savedBuyRate match {
      case Some(rate) => rate
      case None => sys.error("Market is not open. Try calling open()")
      //case None =>
       // update()
        //lastBuyRate
    }

    def sell(amount: Double, currency: String): Transaction =
      if (amount <= 0) sys.error(s"Cannot sell $amount BTCs") else
      new Transaction(-amount, SellCut * amount * lastBuyRate, time, 
          currency)

    def buy(amount: Double, currency: String): Transaction =
      if (amount <= 0) sys.error(s"Cannot buy $amount BTCs") else
      new Transaction(amount, -amount * lastBuyRate, time, currency)

    def quoteToSell(amount: Double, currency: String): Transaction =
      new Transaction(-amount, SellCut * amount * lastBuyRate, time, 
          currency)

    def quoteToBuy(amount: Double, currency: String): Transaction =
      new Transaction(amount, -amount * lastBuyRate, time, currency)

    def quoteToSellCash(amount: Double, currency: String): Transaction =
      quoteToSell(amount / SellCut / lastBuyRate, currency)

    def quoteToBuyCash(amount: Double, currency: String): Transaction =
      quoteToBuy(amount / lastBuyRate, currency)

    def update(): Unit =
      if (this.iterator.hasNext) {
        savedBuyRate = Some(this.iterator.next())
      }

    def isOpen(): Boolean = savedBuyRate match {
      case Some(_) => true
      case None => false
    }

    def open(): Unit = update()
  }
}
