import market.Market
import defs._
import scala.util.Random.nextBoolean

package trader {
  class RandomTrader(val m: Market, var cash: Double, val currency: String)
      extends SingleTrader {
    var bitcoins: Double = 0;
    private val bitcoin_delta = 0.01;
    private var shouldBuy = false

    def update(): Unit = { shouldBuy = nextBoolean }

    def amountToSell(): Double = if (!shouldBuy || cash == 0.0)
      bitcoin_delta else 0.0

    def amountToBuy(): Double = if (shouldBuy || bitcoins == 0.0)
      bitcoin_delta else 0.0

    def updateAfterSell(trans: Transaction): Unit = ()

    def updateAfterBuy(trans: Transaction): Unit = ()
  }

  object RandomTraderFactory extends SingleTraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): SingleTrader =
      new RandomTrader(m, cash, currency)

    override def toString = "Random Trader"
  }
}
