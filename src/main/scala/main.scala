import market._
import trader._
import utils._
import plotter.Plotter
import scala.util.Random.setSeed
import scala.util.Random.nextInt
import scala.util.Random.nextDouble
import heuristics._

object MoneyMaker {
  // Global settings
  val currency = "USD"

  val capital = 100
  val initBTCs = 0.0
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
  val minRisingSlope: Double = 0.10
  val maxDroppingSlope: Double = 2.4
  val minTurnChange: Double = 1.233
  //(19,0.10394231972788748,2.446649732560296,1.2337791754662915)
  val cheatInit = (15,0.0,2.8651634826652685,1.2564524811050153)

  // Params for CoinDesk
  val nDrop: Int = 0
  val nDropFromEnd: Int = 50

  type Lold = List[List[Double]]

  val cdMarket = new CoinDeskMarket(nDrop, nDropFromEnd)
  val histCBMarket = new HistoricalMarket(CoinbaseMarket)
  val allMarkets: List[FakeMarket] =
    List(
      RandomMarket
      , ConstantMarket
      , SinMarket
      , NoisyMarket(SinMarket)
      , CosMarket
      , NoisyMarket(CosMarket)
      , cdMarket
      , histCBMarket
    )
  allMarkets foreach (m => m.open())
  val markets: List[FakeMarket] =
    List(
      RandomMarket
    )
  val simpleFactories =
    List(
      RandomTraderFactory
      //, StubbornTraderFactory(sellPercent)
      //, ReluctantTraderFactory(maxNUpdates, sellPercent)
      //, LowHighMeanTraderFactory(windowSize, buyPercent, sellPercent)
      //, LowMeanStubbornTraderFactory(windowSize, buyPercent)
      //, LowMeanReluctantTraderFactory(maxNUpdates, windowSize, buyPercent)
      //, TurnTraderFactory(windowSize, minRisingSlope, maxDroppingSlope,
      //    minTurnChange)
    )
  val traderFactories = simpleFactories ::: (simpleFactories flatMap
    (f => List(
      //DistributedTraderFactory(f, nDistributedTraders, higherOrderDelay)
      //, AggregateTraderFactory(f, nDistributedTraders, higherOrderDelay)
      )
    )
  )

  def getNewTraders(): List[Trader] = {
    traderFactories flatMap (factory =>
      (markets map (m =>
        factory.newTrader(m, capital, initBTCs, currency)))
      )
  }

  def simDuration =
    MinSimDuration + nextInt(MaxSimDuration - MinSimDuration + 1)

  /* Returns the average returns of the traders in the list in the order that
   * the traders are in. */
  def avgReturns(getTraders: () => List[Trader]): List[Double] = {
    // Get the returns of all the traders using the same markets each.
    def getReturns(markets: List[Market]): List[Double] = {
      val traders = getTraders()
      (1 to simDuration) foreach
        (_ => {
          markets foreach (m => m.update())
          traders foreach (t => t.tryToTrade())
        })
      traders map (t => t.returns)
    }

    val markets = getMarketSet(getTraders())
    markets foreach (m => m.reset())
    var returns = getReturns(markets)
    2 to NTrials foreach { _ =>
      markets foreach (m => m.reset())
      returns = sumList(returns, getReturns(markets))
    }

    scaleList(returns, 1.0 / NTrials.toDouble)
  }

  // Returns the set of markets that these traders go to. Removes duplicates.
  def getMarketSet(traders: List[Trader]): List[Market] = {
    removeDuplicates(traders map (t => t.m))
  }

  /* Returns the traders from the call to getTraders() after they've ran the
   * simulation. */
  def getRanTraders(getTraders: () => List[Trader]): List[Trader] = {
    val traders = getTraders()
    val markets = getMarketSet(traders)
    markets foreach (m => m.reset())
    (1 to simDuration) foreach
      (_ => {
        markets foreach (m => m.update())
        traders foreach (t => t.tryToTrade())
      })
    traders
  }

  def printDetailTraders(traders: List[Trader]): Unit = {
    traders foreach (t => {
      println(s"$t: went to ${t.m} ${t.nTradesTried} times")
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

  def printReturns(t: Trader, r: Double): Unit =
    println(s"${t.name} returns at ${t.m}: $r")

  type TraderParams = Tuple4[Int, Double, Double, Double]

  /* paramSelect uses heuristic algorithms to figure out the best parameters
   * for selected traders. */
  def paramSelect(
      initSoln: TraderParams,
      newTrader: TraderParams => Trader): TraderParams = {
    val stepSize = 1
    val initTemp = 450.0
    val alpha = .99
    val maxTime = 10000

    def costOf(param: TraderParams): Double = {
      def returnsOf(t: Trader): Double = {
        t.m.reset()
        (1 to simDuration) foreach ( _ => { t.m.update(); t.tryToTrade()})
        t.returns
      }

      val r = returnsOf(newTrader(param))

      /* Don't reward something that doesn't invest. Also, negate so that
       * high returns are good */
      if (r == 100.0) 0.0 else -r
    }

    def neighborOf(param: TraderParams): TraderParams = {
      val p1 = param._1 + stepSize*(nextInt(3) - 1)
      val p2 = param._2 + stepSize*(nextDouble - 0.5)
      val p3 = param._3 + stepSize*(nextDouble - 0.5)
      val p4 = param._4 + stepSize*(nextDouble - 0.5)
      (if (p1 < 0) 0 else p1,
        if (p2 < 0) 0 else p2,
        if (p3 < 0) 0 else p3,
        if (p4 < 0) 0 else p4)
    }

    val SA = new Annealing[TraderParams](
        costOf _,
        neighborOf _,
        initSoln,
        initTemp,
        alpha,
        maxTime)
    SA.run()
    SA.bestSoln
  }

  def heuristicMain(): Unit = {
    def newTrader(ps: TraderParams): Trader = {
      new TurnTrader(
        histCBMarket,
        capital,
        initBTCs,
        currency,
        ps._1,
        ps._2,
        ps._3,
        ps._4)
    }
    val initSoln = (windowSize, minRisingSlope, maxDroppingSlope,
        minTurnChange)
    val params = paramSelect(initSoln, newTrader)
    val trader = getRanTraders(() => List(newTrader(params))).head

    printReturns(trader, trader.returns)
    Plotter.plotTraderHistory(trader)
    println(s"Parameters used = $params")
  }

  def fakeMarketsMain(): Unit = {
    val returns = avgReturns(getNewTraders)
    val traders = getRanTraders(getNewTraders)

    println("Below are the returns with the following parameters:")
    println(s"\tSimulation duration = random value in" +
      s" [$MinSimDuration, $MaxSimDuration]")
    println(s"\tNumber of trials ran = $NTrials")
    (traders zip returns) map { case(t, r) => printReturns(t, r) }
    traders foreach Plotter.plotTraderHistory
    traders.head.history foreach println
  }

  def coinBaseMain(): Unit = {
    histCBMarket.open()
    println(CoinbaseMarket.quoteToSell(1.0, currency))
    println(CoinbaseMarket.quoteToBuy(1.0, currency))
    CoinbaseMarket.history foreach println
    CoinbaseMarket.tradeHistory foreach println
    val h = CoinbaseMarket.history
    val dt = h.head.time - h.tail.head.time
    println(s"dt = $dt")
    val trader = getRanTraders(
        () => List(new RandomTrader(histCBMarket, capital, initBTCs, currency))
      ).head
    Plotter.plotTraderHistory(trader)
  }

  def main(args: Array[String]) {
    setSeed(System.currentTimeMillis)
    //fakeMarketsMain()
    //coinBaseMain()
    heuristicMain()
  }
}
