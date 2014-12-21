package market { 
  // Fake markets use fake data for the buy/sell rate.
  trait FakeMarket extends Market with Iterable[Double] {
    val sellCut: Double = .98
    var lastBuyRate: Double = 0
    def getLastBuyRate(): Double = {
      if (lastBuyRate == 0) {
        lastBuyRate = this.iterator.next()
      }
      lastBuyRate
    }

    def time(): Long = System.currentTimeMillis / 1000

    def buy(amount: Double, currency: String): Transaction =
      new Transaction(amount, amount / getLastBuyRate, currency)
    def sell(amount: Double, currency: String): Transaction =
      new Transaction(amount, sellCut * amount / getLastBuyRate, currency)

    def getRate(factor: Double, currency: String): Tuple2[Double, Long] = {
      lastBuyRate = this.iterator.next();
      (factor * lastBuyRate, time())
    }
    def getBuyRate(currency: String): Tuple2[Double, Long]
      = getRate(1, currency)
    def getSellRate(currency: String): Tuple2[Double, Long]
      = getRate(sellCut, currency)
  }
}
