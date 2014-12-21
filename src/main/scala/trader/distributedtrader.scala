import market.Market
import defs._

package trader {
  // Creates many instances of a trader and uses them together to trade
  class DistributedTrader(factory: TraderFactory, val m: Market,
      var cash: Double, val currency: String) extends Trader{
    var bitcoins: Double = 0
    val MinTimeBtwnTrades: Double = 0.01
    val NTraders = 100
    val traders: List[Trader] = List.range(0, NTraders) map
      (i => factory.newTrader(m, cash / NTraders, currency))

    def sum(l: List[Double]): Double = l match {
      case x::tail => x + sum(tail)
      case Nil => 0
    }

    def update(): Unit = traders foreach (t => t.update())
    def amountToSell: Double = sum(traders map (t => t.amountToSell))
    def amountToBuy: Double = sum(traders map (t => t.amountToBuy))
  }

  object DistributedTraderFactory extends TraderFactory {
    private var _traderFactory: Option[TraderFactory] = None
    def traderFactory = _traderFactory match {
      case Some(factory) => factory
      case None => sys.error("No factory set.")
    }
    def traderFactory_= (t: TraderFactory): Unit = _traderFactory = Some(t)

    def newTrader(m: Market, cash: Double, currency: String): Trader =
        _traderFactory match {
      case Some(factory) => new DistributedTrader(factory, m, cash, currency)
      case None => sys.error("Must set traderFactory before creating traders")
    }

    override def toString = _traderFactory match {
      case Some(factory) => s"Distributed '$factory' Trader"
      case None => s"Generic Distributed Trader"
    }
  }
}
