import market.Market
import defs._
import scala.util.Random.nextBoolean

package trader {
  class RandomTrader(
      val m: Market,
      var cash: Double,
      var bitcoins: Double,
      val currency: String)
    extends SingleTrader {
    private var shouldBuy = false

    def update(): Unit = { shouldBuy = nextBoolean }

    def amountToSell(): Double = if (!shouldBuy) bitcoins else 0.0

    def amountToBuy(): Double = if (shouldBuy) maxBTCsCanBuy else 0.0

    def updateAfterSell(trans: Transaction): Unit = ()

    def updateAfterBuy(trans: Transaction): Unit = ()

    def name = "Random Trader"
  }

  object RandomTraderFactory extends SingleTraderFactory {
    def newTrader(
        m: Market,
        cash: Double,
        btc: Double,
        currency: String): SingleTrader =
      new RandomTrader(m, cash, btc, currency)

    override def toString = "Random Trader Factory"
  }
}
