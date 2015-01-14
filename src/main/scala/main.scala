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
  val sellPercent = 0.03
  val buyPercent = 0.03
  val higherOrderDelay = 15 // number of updates between each subinstance
  val minRisingSlope: Double = 0.10
  val maxDroppingSlope: Double = 2.4
  val minTurnChange: Double = 1.233
  val turnTTParams =
    (19,0.10394231972788748,2.446649732560296,1.2337791754662915)
  val cheatInit = (15,0.0,2.8651634826652685,1.2564524811050153)

  // Params for CoinDesk
  val nDrop: Int = 0
  val nDropFromEnd: Int = 50

  type Lold = List[List[Double]]

  val cdMarket = new CoinDeskMarket(nDrop, nDropFromEnd)
  val histCBMarket = new HistoricalMarket(new CoinbaseMarket())
  val histCBMarket2 = new HistoricalMarket(new CoinbaseMarket(2))
  val histCBMarket3 = new HistoricalMarket(new CoinbaseMarket(3))
  val histCBMarket4 = new HistoricalMarket(new CoinbaseMarket(4))
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
      , histCBMarket2
      , histCBMarket3
      , histCBMarket4
    )
  allMarkets foreach (m => m.open())
  val markets: List[FakeMarket] =
    List(
      //RandomMarket
      cdMarket,
      histCBMarket,
      histCBMarket2,
      histCBMarket3,
      histCBMarket4
    )
  val simpleFactories =
    List(
      //RandomTraderFactory,
      StubbornTraderFactory(sellPercent),
      //ReluctantTraderFactory(maxNUpdates, sellPercent),
      //LowHighMeanTraderFactory(windowSize, buyPercent, sellPercent),
      //LowMeanStubbornTraderFactory(windowSize, buyPercent),
      //LowMeanReluctantTraderFactory(maxNUpdates, windowSize, buyPercent),
      (TurnTraderFactory.apply _).tupled(turnTTParams)
    )
  val traderFactories = simpleFactories ::: (simpleFactories flatMap
    (f => List(
      //DistributedTraderFactory(f, nDistributedTraders, higherOrderDelay)
      //, AggregateTraderFactory(f, nDistributedTraders, higherOrderDelay)
      )
    )
  )

  def traderFromFactory(f: TraderFactory, m: Market): Trader = {
    f.newTrader(m, capital, initBTCs, currency)
  }

  def getNewTraders(): List[Trader] = {
    traderFactories flatMap (factory =>
      (markets map (traderFromFactory(factory, _)))
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

  type TTParams = Tuple4[Int, Double, Double, Double]

  /* paramSelect uses heuristic algorithms to figure out the best parameters
   * for selected traders. */
  def paramSelect(
      initSoln: TTParams,
      newTrader: TTParams => Trader): TTParams = {
    val stepSize = 1
    val initTemp = 450.0
    val alpha = .99
    val maxTime = 1000

    def costOf(param: TTParams): Double = {
      def returnsOf(t: Trader): Double = {
        t.m.reset()
        (1 to simDuration) foreach ( _ => { t.m.update(); t.tryToTrade()})
        t.returns
      }

      val t = newTrader(param)
      val r = returnsOf(t)

      /* Don't reward something that doesn't trade. Also, negate so that
       * high returns are good */
      if (r == 100.0 || t.nBuys == 0 || t.nSells == 0) 0.0 else -r
    }

    def neighborOf(param: TTParams): TTParams = {
      val p1 = param._1 + stepSize*(nextInt(3) - 1)
      val p2 = param._2 + stepSize*(nextDouble - 0.5)
      val p3 = param._3 + stepSize*(nextDouble - 0.5)
      val p4 = param._4 + stepSize*(nextDouble - 0.5)
      (if (p1 < 0) 0 else p1,
        if (p2 < 0) 0 else p2,
        if (p3 < 0) 0 else p3,
        if (p4 < 0) 0 else p4)
    }

    val SA = new Annealing[TTParams](
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
    val hcbm2 = new HistoricalMarket(new CoinbaseMarket(3))
    def newTrader(ps: TTParams): Trader = {
      val f = (TurnTraderFactory.apply _).tupled(ps)
      traderFromFactory(f, hcbm2)
    }
    val params = paramSelect(turnTTParams, newTrader)
    val trader = getRanTraders(() => List(newTrader(params))).head

    printReturns(trader, trader.returns)
    Plotter.plotTraderHistory(trader)
    println(s"Parameters used = $params")
  }

  def fakeMarketsMain(): Unit = {
    val returns = avgReturns(getNewTraders)
    val traders = getRanTraders(getNewTraders)

    println("Below are the average returns with the following parameters:")
    println(s"\tSimulation duration = random value in" +
      s" [$MinSimDuration, $MaxSimDuration]")
    println(s"\tNumber of trials ran = $NTrials")
    (traders zip returns) map { case(t, r) => printReturns(t, r) }
    traders foreach Plotter.plotTraderHistory
    //traders.head.history foreach println
  }

  def coinBaseMain(): Unit = {
    /*
    // Testing things
    val cbm = new CoinbaseMarket()
    println(cmb.quoteToSell(1.0, currency))
    println(cmb.quoteToBuy(1.0, currency))
    cmb.history foreach println
    cmb.tradeHistory foreach println
    val h = cmb.history
    val dt = h.head.time - h.tail.head.time
    println(s"dt = $dt")
    */

    var page = 0
    while (true) {
      page += 1
      val hcbm = new HistoricalMarket(new CoinbaseMarket(page))
      hcbm.open()
      val t = getRanTraders(
          () => List(new TurnTrader(
            hcbm,
            capital,
            initBTCs,
            currency,
            windowSize,
            minRisingSlope,
            maxDroppingSlope,
            minTurnChange))
        ).head
      printReturns(t, t.returns)
    }
    //Plotter.plotTraderHistory(trader)
  }

  // What to run to just trade and try to make money.
  def makeMoneyMain(): Unit = {
    println("Test")
  }

  def main(args: Array[String]) {
    setSeed(System.currentTimeMillis)
    if (args.length > 0) {
      args.head match {
        case "-c" => coinBaseMain()
        case "-f" => fakeMarketsMain()
        case "-h" => heuristicMain()
        case "-r" => makeMoneyMain()
        case _ => {
          println("Option not recognized. Recognized options:"
            +"\n\t-c fake coinbase main"
            +"\n\t-f fake market main"
            +"\n\t-h heuristic main"
            +"\n\t-r real trading main")
        }
      }
    } else {
      println("Please provide an option:"
        +"\n\t-c fake coinbase main"
        +"\n\t-f fake market main"
        +"\n\t-h heuristic main"
        +"\n\t-r real trading main")
    }
  }
}
