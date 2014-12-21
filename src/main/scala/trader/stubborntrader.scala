import market.Market

package trader {
  // A trader that buys immediately if he hasn't bought anything yet.
  // Only sells if he would make a profit.
  class StubbornTrader(val m: Market, var cash: Double, val currency: String)
      extends Trader{
    var bitcoins: Double = 0
    var boughtRate: Option[Double] = None
    var savedSellRate: Double = 0
    var savedBuyRate: Double = 0

    def update(): Unit = { savedSellRate = sellRate; savedBuyRate = buyRate }
    def amountToSell = boughtRate match {
      case Some(rate) => if (savedSellRate > rate) {
        boughtRate = None
        bitcoins
      } else {
        0.0
      }
      case None => 0.0
    }
    def amountToBuy = boughtRate match {
      case Some(rate) => 0.0
      case None => {
        val toBuy = cash / buyRate;
        boughtRate = Some(buyRate);
        toBuy
      }
    }
  }

  object StubbornTraderFactory extends TraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): Trader =
      new StubbornTrader(m, cash, currency)

    override def toString = "Stubborn Trader"
  }
}
