import market._
import trader._

object MoneyMaker {
  val currency = "USD"
  val capital = 100
  val SimDuration = 100; // how long each simulation runs for
  val NTrials = 100; // how many simulations to run

  type Lold = List[List[Double]]

  val markets = List(RandomMarket, SinusoidMarket, SinusoidNoiseMarket)
  val dtf = DistributedTraderFactory
  dtf.traderFactory = ReluctantTraderFactory
  val traderFactories: List[TraderFactory] =
    List(
      RandomTraderFactory,
      StubbornTraderFactory,
      ReluctantTraderFactory,
      dtf)

  // Each list corresponds to one trader type. The different traders within
  // correspond to different markets
  def getNewTraders(): List[List[Trader]] = {
    traderFactories map (factory =>
      (markets map (m =>
        factory.newTrader(m, capital, currency)))
      )
  }

  def main(args: Array[String]) {
    // Get the profits of all the traders using the same markets each.
    def getProfits(): Lold = {
      val traderMarketCombos: List[List[Trader]] = getNewTraders()
      (1 to SimDuration) foreach 
        (i => {
          markets foreach (m => m.update())
          traderMarketCombos foreach
            (traders => traders foreach (t => t.trade()))
        })
      traderMarketCombos map (traders =>
        traders map (t => t.moneyLeft)
      )
    }
    def averageProfits(): Lold = {
      def sum2d(lst1: Lold, lst2: Lold): Lold = {
        (lst1 zip lst2) map {
          case (l1, l2) => (l1 zip l2) map { case (d1, d2) => d1 + d2 }
        }
      }
      def scale2d(lst: Lold, factor: Double): Lold = {
        lst map (l => l map (d => factor * d))
      }
      var profits = getProfits()
      2 to NTrials foreach { _ => profits = sum2d(profits, getProfits) }
      scale2d(profits, 1.0 / NTrials.toDouble)
    }
    def printProfits(profits: Lold): Unit = {
        println("Below are the profits for each trader at each market, with" +
          "the following parameters:")
        println(s"\tSimulation duration = $SimDuration")
        println(s"\tNumber of simulations ran = $NTrials")
        (profits zip traderFactories) map { case (ps, name) =>
        println(s"$name:")
        (ps zip markets) map { case (p, m) =>
          println(s"\t$m: $p") 
        }
      }
    }
    printProfits(averageProfits())
  }
}
