import market.Market
import defs._

package trader {
  // A trader that buys based on one strategy and sells based on another.
  class BuySellTrader(
      val m: Market,
      var cash: Double,
      var bitcoins: Double,
      val currency: String,
      b: Buyer,
      s: Seller)
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
      s.updateAfterSell(trans)
    }

    def updateAfterBuy(trans: Transaction): Unit = {
      b.updateAfterBuy(trans)
      s.updateAfterBuy(trans)
    }

    def name = s"'${b.name}' + '${s.name}' Trader"
  }

  class BuySellTraderFactory(
      bf: BuyerFactory,
      sf: SellerFactory)
    extends SingleTraderFactory {
    def newTrader(
        m: Market,
        cash: Double,
        btc: Double,
        currency: String): SingleTrader = {
      val b = bf.newBuyer(m, cash, btc, currency)
      val s = sf.newSeller(m, cash, btc, currency)
      new BuySellTrader(m, cash, btc, currency, b, s)
    }

    override def toString = s"$bf  +  $sf"
  }
}
