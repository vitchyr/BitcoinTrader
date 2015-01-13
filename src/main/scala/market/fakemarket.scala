import defs._
import scala.collection.mutable.ArrayBuffer

package market { 
  // Fake markets use fake data for the buy/sell prices.
  trait FakeMarket extends Market with Iterable[Double] {
    private val feeRate: Double = 0.01
    private val bankCharge: Double = 0.15
    private var savedBuyRate: Option[Double] = None
    private var _history: ArrayBuffer[BitcoinStat] = new ArrayBuffer()
    private var updateIter = -1

    protected def iterTime: Long = updateIter.toLong

    def zeroTrans(currency: String) =
      new Transaction(0.0, 0.0, iterTime, currency)

    // Reset the state of the class as if it had just been initialized
    def resetState(): Unit

    // Reset the fake market as if we had never used it before
    def reset(): Unit = {
      resetState()
      savedBuyRate = None
      _history = new ArrayBuffer()
      updateIter = -1
    }

    private def lastBuyRate: Double = savedBuyRate match {
      case Some(rate) => rate
      case None => sys.error("Market is not open. Try calling open()" +
        " and then calling update().")
    }

    /* How much cash the user will get if he sells [amount] in cash worth of
     * BTCs. Basically, normal amount - fees.
     *
     * [amount] should be non-negative. */
    def cashFromSell(amount: Double): Double = {
      if (amount < 0) {
        sys.error("Expected non-negative amount but instead got"+s" $amount\n")
      } else {
        amount * (1 - feeRate) - bankCharge
      }
    }

    // Inverse of the above function
    def cashFromSellInv(amount: Double): Double = {
      if (amount < 0) {
        sys.error("Expected non-negative amount but instead got"+s" $amount\n")
      } else {
        (amount + bankCharge) / (1 - feeRate)
      }
    }

    /* How much buying [amount] in cash worth of BTC will actually cost the
     * user. Basically, normal amount + fees
     *
     * [amount] should be non-negative. */
    def cashFromBuy(amount: Double): Double = {
      if (amount < 0) {
        sys.error(s"Expected non-negative amount but instead got $amount\n")
      } else {
        amount * (1 + feeRate) + bankCharge
      }
    }

    // Inverse of the above function
    def cashFromBuyInv(amount: Double): Double = {
      if (amount < 0) {
        sys.error(s"Expected non-negative amount but instead got $amount\n")
      } else {
        (amount - bankCharge) / (1 + feeRate)
      }
    }

    def sell(amount: Double, currency: String): Transaction = {
      if (amount <= 0) sys.error(s"Cannot sell $amount BTCs") else
      quoteToSell(amount, currency)
    }

    def buy(amount: Double, currency: String): Transaction = {
      if (amount <= 0) sys.error(s"Cannot buy $amount BTCs") else
      quoteToBuy(amount, currency)
    }

    def quoteToSell(amount: Double, currency: String): Transaction = {
      if (amount < 0) sys.error(s"Cannot sell $amount BTCs") else
      if (amount == 0) zeroTrans(currency) else
      new Transaction(-amount, cashFromSell(amount * lastBuyRate), iterTime,
          currency)
    }

    def quoteToBuy(amount: Double, currency: String): Transaction = {
      if (amount < 0) sys.error(s"Cannot buy $amount BTCs") else
      if (amount == 0) zeroTrans(currency) else
      new Transaction(amount, -cashFromBuy(amount * lastBuyRate), iterTime,
          currency)
    }

    def quoteToSellCash(amount: Double, currency: String): Transaction = {
      val btcAmnt = cashFromSellInv(amount / lastBuyRate)
      if (btcAmnt <= 0) zeroTrans(currency) else
      quoteToSell(btcAmnt, currency)
    }

    def quoteToBuyCash(amount: Double, currency: String): Transaction = {
      val btcAmnt = cashFromBuyInv(amount) / lastBuyRate
      if (btcAmnt <= 0) zeroTrans(currency) else
      quoteToBuy(btcAmnt, currency)
    }

    def update(): Unit = {
      updateIter += 1
      if (this.iterator.hasNext) {
        val rate = this.iterator.next()
        _history append (new BitcoinStat(iterTime, rate))
        savedBuyRate = Some(rate)
      } else {
        savedBuyRate = None
      }
    }

    def isOpen: Boolean = savedBuyRate match {
      case Some(_) => true
      case None => false
    }

    def open(): Unit = ()

    def history: MarketHistory = _history.toList
  }
}
