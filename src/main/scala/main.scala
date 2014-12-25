import market._
import trader._
import plotter.Plotter
import scala.util.Random.setSeed
import scala.util.Random.nextInt

object MoneyMaker {
  // Global settings
  val currency = "USD"
  val capital = 100000
  val MinSimDuration = 1500; // min time a simulation runs for
  val MaxSimDuration = 2000;
  val NTrials = 1; // how many simulations to run

  // Parameters for factories
  val maxNumUpdates = 100 // numberof updates until reluctant trader gives up
  val nDistributedTraders = 10
  val meanWindowSize = 30
  val sellPercent = 0.03
  val buyPercent = 0.03
  val higherOrderDelay = 5 // number of updates between each subinstance

  type Lold = List[List[Double]]

  val markets: List[FakeMarket] =
    List(
      //RandomMarket
      //, SinMarket
      //, NoisyMarket(SinMarket)
      //, CosMarket
      //, NoisyMarket(CosMarket)
      CoinDeskMarket
    )
  val simpleFactories =
    List(
      RandomTraderFactory
      , StubbornTraderFactory
      , ReluctantTraderFactory(maxNumUpdates)
      , LowHighMeanTraderFactory(meanWindowSize, buyPercent, sellPercent)
      , LowMeanStubbornTraderFactory(meanWindowSize, buyPercent)
      , LowMeanReluctantTraderFactory(maxNumUpdates, meanWindowSize, buyPercent)
    )
  val traderFactories = simpleFactories ::: (simpleFactories flatMap
    (f => List(
      DistributedTraderFactory(f, nDistributedTraders, higherOrderDelay)
      //, AggregateTraderFactory(f, nDistributedTraders, higherOrderDelay)
      )
    )
  )

  // Each list corresponds to one trader type. The different traders within
  // correspond to different markets
  def getNewTraders(): List[List[Trader]] = {
    traderFactories map (factory =>
      (markets map (m =>
        factory.newTrader(m, capital, currency)))
      )
  }

  def simDuration =
    MinSimDuration + nextInt(MaxSimDuration - MinSimDuration + 1)

  /* main checks the profit made by each trader on different markets. This is
   * averaged of [NTrials]. Each simulation lasts [simDuration] time steps,
   * which is designed to be random so that different results happen from
   * markets like the sinusoidal markets. */
  def main(args: Array[String]) {
    setSeed(System.currentTimeMillis)

    // Get the profits of all the traders using the same markets each.
    def getProfits(): Lold = {
      val traderMarketCombos: List[List[Trader]] = getNewTraders()
      (1 to simDuration) foreach 
        (_ => {
          markets foreach (m => m.update())
          traderMarketCombos foreach
            (traders => traders foreach (t => t.tryToTrade()))
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
      markets foreach (m => m.open())
      var profits = getProfits()
      2 to NTrials foreach { _ =>
        markets foreach (m => m.reset())
        profits = sum2d(profits, getProfits)
      }

      scale2d(profits, 1.0 / NTrials.toDouble)
    }

    def printProfits(profits: Lold): Unit = {
      println("Below are the returns for each trader at each market, with" +
        "the following parameters:")
      println(s"\tSimulation duration = random value in" +
        s" [$MinSimDuration, $MaxSimDuration]")
      println(s"\tNumber of simulations ran = $NTrials")
      (profits zip traderFactories) map { case (ps, name) =>
        println(s"$name:")
        (ps zip markets) map { case (p, m) =>
          println(s"\t$m: "+f"${(100*p/capital)}%3.2f%%") 
        }
      }
    }

    // Get one instance of the traders that have traded.
    def getSampleTraders(): List[Trader] = {
      markets foreach (m => m.reset())
      val traderMarketCombos: List[List[Trader]] = getNewTraders()
      (1 to simDuration) foreach
        (_ => {
          markets foreach (m => m.update())
          traderMarketCombos foreach
            (traders => traders foreach (t => t.tryToTrade()))
        })
      traderMarketCombos.flatten
    }

    def displaySample(traders: List[Trader]): Unit = {
      /*
      traders foreach (t => {
        Plotter.plotTraderHistory(t)
      })
      traders foreach (t =>
        println(s"$t: made ${t.nTradesTried} trade calls at ${t.m}"))
*/
      Plotter.plotTraderHistory(traders.head)
    }

    printProfits(averageProfits())
    displaySample(getSampleTraders())
  }
}
