import market._
import trader._

object MoneyMaker {
  val currency = "USD"
  val capital = 100
  val sim_duration = 1000; // how long each simulation runs for
  val n_trials = 100; // how mayn simulations to run
  def main(args: Array[String]) {
    val markets = List(RandomMarket, SinusoidMarket)
    // Get the profits of all the traders
    def getProfits(): List[Double] = {
      val traders = markets map (market => new RandomTrader(market, capital, currency))
      def getProfit(trader: Trader): Double = {
        1 to sim_duration foreach { _ => trader.trade() }
        trader.getMoneyLeft
      }
      traders map getProfit
    }
    var profits = getProfits()
    2 to n_trials foreach { _ =>
      profits = (getProfits() zip profits) map {
        case (sum, next) => sum + next
      }
    }
    profits = profits map (x => x/n_trials)
    (profits zip markets) map {
      case (perf, market) => println(s"$market money left: $perf")
    }
  }
}
