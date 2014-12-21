import market.Market
import scala.util.Random.nextBoolean

package trader {
  class RandomTrader(val m: Market, var cash: Double, val currency: String)
      extends Trader{
    var bitcoins: Double = 0;
    val bitcoin_delta = 0.01;
    var shouldBuy = false

    def update(): Unit = { shouldBuy = nextBoolean }
    def amountToSell(): Double = if (!shouldBuy || cash == 0.0)
      bitcoin_delta else 0.0
    def amountToBuy(): Double = if (shouldBuy || bitcoins == 0.0)
      bitcoin_delta else 0.0
  }

  object RandomTraderFactory extends TraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): Trader =
      new RandomTrader(m, cash, currency)

    override def toString = "Random Trader"
  }
}
