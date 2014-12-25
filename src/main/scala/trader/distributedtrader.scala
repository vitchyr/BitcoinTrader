import market.Market
import defs._
import scala.collection.mutable.ArrayBuffer

package trader {
  /* Creates many instances of a trader. Divides money among them and lets them
   * trade.
   *
   * Each subinstance of traders doesn't start immediately. They have [delay]
   * updates between each starts. Set [delay] to start all subtraders
   * immediately. */
  class DistributedTrader(
      val m: Market,
      var cash: Double,
      val currency: String,
      factory: TraderFactory,
      nTraders: Int,
      delay: Int)
    extends Trader {
    var bitcoins: Double = 0

    private val allTraders: Array[Trader] = (List.range(0, nTraders) map
      (i => factory.newTrader(m, cash / nTraders, currency))).toArray
    private var traders: ArrayBuffer[Trader] = new ArrayBuffer()
    if (delay == 0) traders appendAll allTraders

    private var nUpdates: Int = 0

    def trade(): Unit = {
      if (delay != 0 && (nUpdates % delay) == 0) {
        if (nUpdates < delay * allTraders.length) {
          traders.append(allTraders(nUpdates/delay))
        }
      }
      nUpdates += 1
      traders foreach (t => t.trade())
    }

    def history: List[Transaction] = (traders flatMap (t => t.history)).toList

    def moneyLeft = {
      def sum(xs: ArrayBuffer[Double]): Double = (0.0 /: xs)(_+_)
      sum(traders map (t => t.moneyLeft))
    }

    def name =
      if (traders.length == 0)
        "Generic Distributed Trader"
      else
        s"Distributed '${traders.head.name}' Trader"
  }

  class DistributedTraderFactory(
      f: TraderFactory,
      nTraders: Int,
      delay: Int)
    extends TraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): Trader =
      new DistributedTrader(m, cash, currency, f, nTraders, delay)

    override def toString = s"Distributed '$f' Trader"
  }

  object DistributedTraderFactory {
    def apply(f: TraderFactory, nTraders: Int, delay: Int) =
      new DistributedTraderFactory(f, nTraders, delay)
  }
}
