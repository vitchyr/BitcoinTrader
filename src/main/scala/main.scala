import market._
import trader._

object MoneyMaker {
  val currency = "USD"
  val capital = 100
  val sim_duration = 1000; // how long each simulation runs for
  val n_trials = 100; // how many simulations to run

  type Lold = List[List[Double]]

  val markets = List(RandomMarket, SinusoidMarket, SinusoidNoiseMarket)
  val traderNames = List("RandomTrader", "StubbornTrader")
  def getNewTraders(): List[List[Trader]] = {
    List(
      markets map (m => new RandomTrader(m, capital, currency)),
      markets map (m => new StubbornTrader(m, capital, currency)))
  }

  def main(args: Array[String]) {
    // Get the profits of all the traders
    def getProfits(): Lold = {
      def getProfit(trader: Trader): Double = {
        1 to sim_duration foreach { _ => trader.trade() }
        trader.getMoneyLeft
      }
      getNewTraders() map (traders => traders map getProfit)
    }
    def sum2d(lst1: Lold, lst2: Lold): Lold = {
      (lst1 zip lst2) map {
        case (l1, l2) => (l1 zip l2) map { case (d1, d2) => d1 + d2 }
      }
    }
    def scale2d(lst: Lold, factor: Double): Lold = {
      lst map (l => l map (d => factor * d))
    }
    var profits = getProfits()
    2 to n_trials foreach { _ => profits = sum2d(profits, getProfits) }
    profits = scale2d(profits, 1.0 / n_trials.toDouble)
    def printProfits(profits: Lold): Unit = {
      (profits zip traderNames) map { case (ps, name) =>
        println(s"$name:")
        (ps zip markets) map { case (p, m) =>
          println(s"\tProfit at $m: $p") 
        }
      }
    }
    printProfits(profits)
  }
}
