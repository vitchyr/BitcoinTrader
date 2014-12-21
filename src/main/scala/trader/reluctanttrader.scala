import market.Market
import defs._

package trader {
  // A trader that buys immediately if he hasn't bought anything yet.
  // Only sells if he would make a profit, or if it's been a while
  class ReluctantTrader(val m: Market, var cash: Double, val currency: String)
      extends Trader{
    var bitcoins: Double = 0
    var boughtInfo: Option[BitcoinInfo] = None
    var savedSellInfo: Option[BitcoinInfo] = None
    val MaxTime: Double = 0.01

    def update(): Unit = { savedSellInfo = Some(sellInfo) }
    def amountToSell: Double = (savedSellInfo, boughtInfo) match {
      case (Some(newSellInfo), Some(savedBoughtInfo)) =>
        if (newSellInfo.price > savedBoughtInfo.price ||
            (newSellInfo.time - savedBoughtInfo.time) > MaxTime) {
          boughtInfo = None
          bitcoins
        } else {
          0.0
        }
      case _ => 0.0
    }
    def amountToBuy: Double = boughtInfo match {
      case Some(info) => 0.0
      case None =>
        val info = buyInfo
        boughtInfo = Some(info)
        cash / info.price
    }
  }

  object ReluctantTraderFactory extends TraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): Trader =
      new ReluctantTrader(m, cash, currency)

    override def toString = "Reluctant Trader"
  }
}
