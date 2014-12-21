import market.Market
import scala.util.Random.nextBoolean

package trader {
  // A trader that buys immediately if he hasn't bought anything yet.
  // Only sells if he would make a profit.
  class StubbornTrader(val m: Market, var cash: Double, val currency: String)
      extends Trader{
    var bitcoins: Double = 0;
    var boughtRate: Option[Double] = None

    def shouldSell(buyRate: Double): Boolean = getSellRate() > buyRate

    def trade(): Unit = boughtRate match {
      case Some(buyRate) => {
        if (shouldSell(buyRate)) {
          sell(bitcoins) 
        }
      }
      case None => {
        buy(cash / getBuyRate)
      }
    }
  }
}
