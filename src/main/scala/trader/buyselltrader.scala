import market.Market
import defs._

package trader {
  // A trader that buys based on one strategy and sells based on another.
  class BuySellTrader(
      val m: Market,
      var cash: Double,
      var bitcoins: Double,
      val currency: String,
      b: SingleTrader,
      s: SingleTrader)
    extends SingleTrader {
    private var shouldBuy = false

    def update(): Unit = {
      b.update()
      s.update()
    }

    def amountToBuy: Double = b.amountToBuy

    def amountToSell: Double = s.amountToSell

    def updateAfterSell(trans: Transaction): Unit = {
      b.updateAfterSell(trans)
      b.updateBank(trans)
      s.updateAfterSell(trans)
      s.updateBank(trans)
    }

    def updateAfterBuy(trans: Transaction): Unit = {
      b.updateAfterBuy(trans)
      b.updateBank(trans)
      s.updateAfterBuy(trans)
      s.updateBank(trans)
    }

    def name = s"'${b.name}' Buyer + '${s.name}' Seller"
  }

  class BuySellTraderFactory(
      bf: SingleTraderFactory,
      sf: SingleTraderFactory)
    extends SingleTraderFactory {
    def newTrader(
        m: Market,
        cash: Double,
        btc: Double,
        currency: String): SingleTrader = {
      val b = bf.newTrader(m, cash, btc, currency)
      val s = sf.newTrader(m, cash, btc, currency)
      new BuySellTrader(m, cash, btc, currency, b, s)
    }

    override def toString = s"$bf  +  $sf"
  }
}
