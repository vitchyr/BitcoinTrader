import market.Market
import defs._
import scala.collection.mutable.ArrayBuffer

package trader {
  /* Creates many instances of a trader and uses them to decide how much to
   * trade and sell. This is similar to DistributedTrader, but this makes one
   * lump buy/sell, rather than many small buy/sells.
   *
   * Each subinstance of traders doesn't start immediately. They have [delay]
   * updates between each starts. Set [delay] to start all subtraders
   * immediately. */
  class AggregateTrader(
      val m: Market,
      var cash: Double,
      var bitcoins: Double,
      val currency: String,
      factory: SingleTraderFactory,
      nTraders: Int,
      delay: Int)
    extends SingleTrader {
    /* The arrays keep track of how much each trader thinks their BTC/cash
     * should change. So if dBtcFromSell(X) = Y, then trader(X) thinks his
     * BTC amount should change by Y from a sale. */
    private var dBtcFromSell: Array[Double] = new Array(nTraders)
    private var dBtcFromBuy: Array[Double] = new Array(nTraders)
    private var dCashFromSell: Array[Double] = new Array(nTraders)
    private var dCashFromBuy: Array[Double] = new Array(nTraders)

    private val allTraders: Array[SingleTrader] = (List.range(0, nTraders) map
      (i => factory.newTrader(
        m, cash / nTraders, bitcoins / nTraders, currency
      ))).toArray
    private var traders: ArrayBuffer[SingleTrader] = new ArrayBuffer()
    if (delay == 0) traders appendAll allTraders

    private var nUpdates: Int = 0

    def sum(xs: ArrayBuffer[Double]): Double = (0.0 /: xs)(_+_)

    /* Figure out how much they want to sell/buy each. Aggregate these to
     * figure out how much to buy/sell in total*/
    def update(): Unit = {
      if (delay != 0 && (nUpdates % delay) == 0) {
        if (nUpdates < delay * allTraders.length) {
          traders.append(allTraders(nUpdates/delay))
        }
      }
      nUpdates += 1
      (List.range(0, nTraders) zip traders) foreach { case(i, t) =>
        def fakeSell(amount: Double): Unit = {
          if (amount <= 0 || amount > t.getBtc) {
            if (amount < 0) {
              sys.error(s"Can't sell $amount BTCs - $this")
            }
            return ()
          }
          val trans = sellQuote(amount)
          t.updateBank(trans)
          t.updateAfterSell(trans)
        }

        def fakeBuy(amount: Double): Unit = {
          if (amount <= 0 || priceOf(amount) > t.getCash) {
            if (amount < 0) {
              sys.error(s"Can't buy $amount BTCs - $this")
            }
            return ()
          }
          val trans = buyQuote(amount)
          t.updateBank(trans)
          t.updateAfterBuy(trans)
        }

        t.update()
        val toSell = t.amountToSell
        val toBuy = t.amountToBuy
        fakeSell(toSell)
        fakeBuy(toBuy)
      }
    }

    def amountToSell = sum(traders map (t => t.amountToSell))
    def amountToBuy = sum(traders map (t => t.amountToBuy))

    def updateAfterSell(trans: Transaction): Unit = ()

    def updateAfterBuy(trans: Transaction): Unit = ()

    override def moneyLeft = sum(traders map (t => t.moneyLeft))

    def name =
      if (traders.length == 0)
        "Generic Aggregate Trader"
      else
        s"Aggregate '${traders.head.name}' Trader"
  }

  class AggregateTraderFactory(
      f: SingleTraderFactory,
      nTraders: Int,
      delay: Int)
    extends SingleTraderFactory {
    def newTrader(
        m: Market,
        cash: Double,
        btc: Double,
        currency: String): SingleTrader =
      new AggregateTrader(m, cash, btc, currency, f, nTraders, delay)

    override def toString = s"Aggregate '$f' Trader"
  }

  object AggregateTraderFactory {
    def apply(f: SingleTraderFactory, nTraders: Int, delay: Int) =
      new AggregateTraderFactory(f, nTraders, delay)
  }
}
