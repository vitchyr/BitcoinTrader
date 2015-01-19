import defs._
import utils._
import scala.io.Source
import com.coinbase.api.entity.{Transfer, Account}
import com.coinbase.api.{Coinbase, CoinbaseBuilder}
import org.joda.money.{Money, CurrencyUnit}
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

package market { 
  // A wrapper around the coinbase java API
  class CoinbaseMarket(pageNum: Int = 1) extends Market {
    private val Currency = "USD"
    private val CUnit = CurrencyUnit.USD
    private var cb: Coinbase = null // the CoinbaseBuilder
    private var _isOpen = false
    private var _history: ArrayBuffer[BitcoinStat] = new ArrayBuffer()

    protected var _spotPrice: Option[Double] = None

    // IMPORTANT: Update this as needed
    private val transFee = 0.01
    private val bankFee = 0.15
    private def cashToBtc(c: Double): Double = c * (1 - transFee) - bankFee

    def sell(amount: Double, currency: String): Transaction = {
      println(s"Fake sell: ${quoteToSell(amount, currency)}")
      quoteToSell(amount, currency)
    }

    def buy(amount: Double, currency: String): Transaction = {
      println(s"Fake buy: ${quoteToBuy(amount, currency)}")
      quoteToBuy(amount, currency)
    }

    def quoteToSell(amount: Double, currency: String): Transaction = {
      val q = cb.getSellQuote(Money.parse(s"BTC $amount"))
      /*
      val fees = q.getFees()
      println(s"Sell quote fees = $fees")
      println(s"subtotal = ${q.getSubtotal()}")
      println(s"total = ${q.getTotal()}")
      */
      new Transaction(
        -amount,
        q.getTotal().getAmount().doubleValue(),
        time,
        currency)
    } 
    def quoteToBuy(amount: Double, currency: String): Transaction = {
      val q = cb.getBuyQuote(Money.parse(s"BTC $amount"))
      new Transaction(
        amount,
        -q.getTotal().getAmount().doubleValue(),
        time,
        currency)
    }

    def quoteToSellCash(amount: Double, currency: String): Transaction =
      quoteToSell(cashToBtc(amount), currency)

    def quoteToBuyCash(amount: Double, currency: String): Transaction =
      quoteToBuy(cashToBtc(amount), currency)

    def update(): Unit = {
      val p = cb.getSpotPrice(CUnit)
      val s = new BitcoinStat(time, p.getAmount().doubleValue())
      var _spotPrice = Some(s.price)
      if (!_history.isEmpty && (s.time - _history.head.time).abs < minDTime) {
        _isOpen = false
      } else {
        _history.prepend(s)
        _isOpen = true
      }
    }

    def isOpen(): Boolean = _isOpen

    def open: Unit = {
      val secretData =
        (for(l <- Source.fromFile(".secret").getLines()) yield l).toList
      val key = secretData.head.split("=").last
      val secret = secretData.last.split("=").last
      cb = new CoinbaseBuilder()
                          .withApiKey(key, secret)
                          .build()

      // History given is in reverse chronological order
      _history ++= (for (p <- cb.getHistoricalPrices(pageNum).toList.reverse)
        yield {
          new BitcoinStat(
            p.getTime().getMillis(),
            p.getSpotPrice().getAmount().doubleValue())
        })
      _isOpen = true
    }

    def history: MarketHistory = _history.toList

    def tradeHistory: TraderHistory = {
      for (t <- cb.getTransfers().getTransfers().toList) yield {
        if (t.getType().toString() == Transfer.Type.SELL.toString()) {
          new Transaction(
            -t.getBtc().getAmount().doubleValue(),
            t.getTotal().getAmount().doubleValue(),
            t.getPayoutDate().getMillis(),
            Currency)
        } else { // bought BTC
          new Transaction(
            t.getBtc().getAmount().doubleValue(),
            -t.getTotal().getAmount().doubleValue(),
            t.getPayoutDate().getMillis(),
            Currency)
        }
      }
    }

    def reset(): Unit = {
      cb = null
      _isOpen = false
      _history = new ArrayBuffer()
      _spotPrice = None
    }

    override def toString = s"Coinbase Market, page $pageNum"

    /****** New Methods ******/
    // Get my account, assuming there's only one.
    def account: Account = cb.getAccounts().getAccounts().get(0)

    // How much BTC is in the coinbase account to start with
    def cash: Double = {
      val c = 100.00 + (tradeHistory map (_.dCash)).sum
      if (c < 0) 0.0 else c
    }

    // How much cash is in the coinbase account to start with
    def bitcoins: Double = account.getBalance().getAmount.doubleValue()
  }
}
