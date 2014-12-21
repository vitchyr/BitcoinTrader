import market.Market
import scala.util.Random.nextBoolean

package trader {
  class RandomTrader(val m: Market, var cash: Double, val currency: String)
      extends Trader{
    var bitcoins: Double = 0;
    val bitcoin_delta = 0.01;

    def trade(): Unit = {
      if (nextBoolean || bitcoins == 0) {
        buy(bitcoin_delta)
      } else {
        sell(bitcoin_delta)
      }
    }
  }
}
