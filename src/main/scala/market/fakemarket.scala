import defs._

package market { 
  // Fake markets use fake data for the buy/sell rate.
  trait FakeMarket extends Market with Iterable[Double] {
    val SellCut: Double = .98
    var lastBuyRate: Option[Double] = None

    // in seconds
    def time(): Double = System.currentTimeMillis.toDouble / 1000

    def getLastBuyRate(): Double = lastBuyRate match {
      case Some(rate) => rate
      case None => {
        val newRate = this.iterator.next()
        lastBuyRate = Some(newRate)
        newRate
      }
    }

    def buy(amount: Double, currency: String): Transaction =
      new Transaction(amount, amount * getLastBuyRate(), currency)

    def sell(amount: Double, currency: String): Transaction =
      new Transaction(amount, SellCut * amount * getLastBuyRate(), currency)

    def getRate(factor: Double, currency: String): BitcoinInfo =
      new BitcoinInfo(factor * getLastBuyRate(), time(), currency)

    def getBuyInfo(currency: String): BitcoinInfo
      = getRate(1, currency)

    def getSellInfo(currency: String): BitcoinInfo
      = getRate(SellCut, currency)

    def update(): Unit =
      lastBuyRate = Some(this.iterator.next())
  }
}
