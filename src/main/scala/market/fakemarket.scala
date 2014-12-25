import defs._
import scala.collection.mutable.ArrayBuffer

package market { 
  // Fake markets use fake data for the buy/sell prices.
  trait FakeMarket extends Market with Iterable[Double] {
    private val SellCut: Double = .99
    private var savedBuyRate: Option[Double] = None
    private var _history: ArrayBuffer[BitcoinStat] = new ArrayBuffer()
    private var updateIter = -1

    // Reset the state of the class as if it had just been initialized
    def resetState(): Unit

    // Reset the fake market as if we had never used it before
    def reset(): Unit = {
      resetState()
      savedBuyRate = None
      _history = new ArrayBuffer()
      updateIter = -1
      open()
    }

    // in milliseconds
    def time(): Long = System.currentTimeMillis

    private def lastBuyRate: Double = savedBuyRate match {
      case Some(rate) => rate
      case None => sys.error("Market is not open. Try calling open()" +
        " and then calling update().")
    }

    def sell(amount: Double, currency: String): Transaction =
      if (amount <= 0) sys.error(s"Cannot sell $amount BTCs") else
      new Transaction(-amount, SellCut * amount * lastBuyRate, updateIter,
          currency)

    def buy(amount: Double, currency: String): Transaction =
      if (amount <= 0) sys.error(s"Cannot buy $amount BTCs") else
      new Transaction(amount, -amount * lastBuyRate, updateIter, currency)

    def quoteToSell(amount: Double, currency: String): Transaction =
      new Transaction(-amount, SellCut * amount * lastBuyRate, updateIter,
          currency)

    def quoteToBuy(amount: Double, currency: String): Transaction =
      new Transaction(amount, -amount * lastBuyRate, updateIter, currency)

    def quoteToSellCash(amount: Double, currency: String): Transaction =
      quoteToSell(amount / SellCut / lastBuyRate, currency)

    def quoteToBuyCash(amount: Double, currency: String): Transaction =
      quoteToBuy(amount / lastBuyRate, currency)

    def update(): Unit = {
      updateIter += 1
      if (this.iterator.hasNext) {
        val rate = this.iterator.next()
        _history append (new BitcoinStat(updateIter.toDouble, rate))
        savedBuyRate = Some(rate)
      } else {
        savedBuyRate = None
      }
    }

    def isOpen(): Boolean = savedBuyRate match {
      case Some(_) => true
      case None => false
    }

    def open(): Unit = ()

    def history: MarketHistory = _history.toList
  }
}
