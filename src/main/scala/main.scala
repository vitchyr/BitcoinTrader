import market._
import trader._
import plotter.Plotter
import scala.util.Random.setSeed
import scala.util.Random.nextInt
import scala.util.Random.nextDouble
import heuristics._

object MoneyMaker {
  // Global settings
  val currency = "USD"
  val capital = 100000
  val MinSimDuration = 1500; // min time a simulation runs for
  val MaxSimDuration = 2000; // this only matters for fake infinite markets
  val NTrials = 1; // how many simulations to run

  // Parameters for factories
  val maxNUpdates = 30 // numberof updates until reluctant trader gives up
  val nDistributedTraders = 10
  val windowSize = 19
  val sellPercent = 0.00
  val buyPercent = 0.03
  val higherOrderDelay = 15 // number of updates between each subinstance
  val minRisingSlope = 0.10394231972788748
  val maxDroppingSlope = 2.446649732560296
  val minTurnChange = 1.2337791754662915
  //(19,0.10394231972788748,2.446649732560296,1.2337791754662915)

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
      //, StubbornTraderFactory(sellPercent)
      //, ReluctantTraderFactory(maxNUpdates, sellPercent)
      //, LowHighMeanTraderFactory(windowSize, buyPercent, sellPercent)
      //, LowMeanStubbornTraderFactory(windowSize, buyPercent)
      //, LowMeanReluctantTraderFactory(maxNUpdates, windowSize, buyPercent)
      , TurnTraderFactory(windowSize, minRisingSlope, maxDroppingSlope,
          minTurnChange)
    )
  val traderFactories = simpleFactories ::: (simpleFactories flatMap
    (f => List(
      //DistributedTraderFactory(f, nDistributedTraders, higherOrderDelay)
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

  /* evaluateTraders evaluates each trader on different markets. This is
   * averaged of [NTrials]. Each simulation lasts [simDuration] time steps,
   * which is designed to be random so that different results happen from
   * markets like the sinusoidal markets. */
  def evaluateTraders(): Unit = {
    // Get the returns of all the traders using the same markets each.
    def getReturns(): Lold = {
      val traderMarketCombos: List[List[Trader]] = getNewTraders()
      (1 to simDuration) foreach 
        (_ => {
          markets foreach (m => m.update())
          traderMarketCombos foreach
            (traders => traders foreach (t => t.tryToTrade()))
        })
      traderMarketCombos map (traders =>
        traders map (t => t.returns)
      )
    }

    def averageReturns(): Lold = {
      def sum2d(lst1: Lold, lst2: Lold): Lold = {
        (lst1 zip lst2) map {
          case (l1, l2) => (l1 zip l2) map { case (d1, d2) => d1 + d2 }
        }
      }

      def scale2d(lst: Lold, factor: Double): Lold = {
        lst map (l => l map (d => factor * d))
      }
      markets foreach (m => m.open())
      var returns = getReturns()
      2 to NTrials foreach { _ =>
        markets foreach (m => m.reset())
        returns = sum2d(returns, getReturns())
      }

      scale2d(returns, 1.0 / NTrials.toDouble)
    }

    def printReturns(returns: Lold): Unit = {
      println("Below are the returns for each trader at each market, with" +
        "the following parameters:")
      println(s"\tSimulation duration = random value in" +
        s" [$MinSimDuration, $MaxSimDuration]")
      println(s"\tNumber of simulations ran = $NTrials")
      (returns zip traderFactories) map { case (rs, name) =>
        println(s"$name:")
        (rs zip markets) map { case (r, m) =>
          println(s"\t$m: "+f"$r%3.2f%%")
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
      traders foreach (t => {
        Plotter.plotTraderHistory(t)
      })
      traders foreach (t => {
        println(s"$t: made ${t.nTradesTried} trade calls at ${t.m}")
        if (t.cashLostToRounding > 0.0) {
          println(s"[Warning] Due to rounding, ${t.name} lost" +
            f" ${t.cashLostToRounding}%.2f" +
            s" of ${t.currency}.")
        }
        if (t.btcLostToRounding > 0.0) {
          println(s"[Warning] Due to rounding, ${t.name} lost" +
            f"${t.btcLostToRounding}%.2f" +
             " of BTC.")
        }
      })
    }

    printReturns(averageReturns())
    displaySample(getSampleTraders())
  }

  /* paramSelect uses heuristic algorithms to figure out the best parameters
   * for selected traders. */
  def paramSelect(): Unit = {
    type TTParam = Tuple4[Int, Double, Double, Double]

    def costOf(param: TTParam): Double = {
      def returnsOf(t: Trader): Double = {
        t.m.reset()
        (1 to simDuration) foreach ( _ => { t.m.update(); t.tryToTrade()})
        t.returns 
      }

      val r = returnsOf(new TurnTrader(
        CoinDeskMarket,
        capital,
        currency,
        param._1,
        param._2,
        param._3,
        param._4)
      )
      /* Don't reward something that doesn't invest. Also, negate so that low
       * is good */
      if (r == 100.0) 0.0 else -r 
    }

    val stepSize = 1

    def neighborOf(param: TTParam): TTParam = {
      val p1 = param._1 + stepSize*(nextInt(3) - 1)
      val p2 = param._2 + stepSize*(nextDouble - 0.5)
      val p3 = param._3 + stepSize*(nextDouble - 0.5)
      val p4 = param._4 + stepSize*(nextDouble - 0.5)
      (if (p1 < 0) 0 else p1,
        if (p2 < 0) 0 else p2,
        if (p3 < 0) 0 else p3,
        if (p4 < 0) 0 else p4)
    }
    
    /*
    val initSoln = (windowSize, minRisingSlope, maxDroppingSlope,
        minRisingSlope)
    */
    val initSoln = (10,0.3850272453276844,0.6953361573251626,0.3580603780969772)
    val initTemp = 450.0
    val alpha = 0.95
    val maxTime = 1000

    val SA = new Annealing[TTParam](
        costOf _,
        neighborOf _,
        initSoln,
        initTemp,
        alpha,
        maxTime)
    SA.run()
    println(SA.bestSoln)
    println(costOf(SA.bestSoln))
  }

  def main(args: Array[String]) {
    setSeed(System.currentTimeMillis)
    evaluateTraders()
    //paramSelect()
  }
}
