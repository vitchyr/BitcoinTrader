import market._
import trader._
import utils._
import defs.minDTime
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

  // Factory Settings
  val maxNUpdates = 30 // numberof updates until reluctant trader gives up
  val nDistributedTraders = 10
  val windowSize = 19
  val sellPercent = 0.03
  val buyPercent = 0.03
  val higherOrderDelay = 15 // number of updates between each subinstance
  val minRisingSlope: Double = 0.10
  val maxDroppingSlope: Double = 2.4
  val minTurnChange: Double = 1.233
  val turnParams =
    //(19,0.10394231972788748,2.446649732560296,1.2337791754662915)
    (3,3.1592458657476055,9.277464784901152,2.692856100058393)
  val meanParams = (10, 1.05, 1.05)
  val cheatInit = (15,0.0,2.8651634826652685,1.2564524811050153)

  // CoinDesk Settings
  val nDrop: Int = 0
  val nDropFromEnd: Int = 50

  // Simulated Annealing Settings
  val stepSize = 3
  val initTemp = 450.0
  val alpha = .99
  val maxTime = 5000

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
      RandomMarket,
      //cdMarket,
      histCBMarket
      //histCBMarket2
      //histCBMarket3,
      //histCBMarket4
    )
  val turn = (TurnTraderFactory.apply _).tupled(turnParams)
  val stubborn = StubbornTraderFactory(sellPercent)
  val mean = LowHighMeanTraderFactory(windowSize, buyPercent, sellPercent)
  val random = RandomTraderFactory
  val simpleFactories =
    List(
      //RandomTraderFactory,
      //StubbornTraderFactory(sellPercent),
      //ReluctantTraderFactory(maxNUpdates, sellPercent),
      LowHighMeanTraderFactory(windowSize, buyPercent, sellPercent),
      //LowMeanStubbornTraderFactory(windowSize, buyPercent),
      //LowMeanReluctantTraderFactory(maxNUpdates, windowSize, buyPercent),
      new BuySellTraderFactory(mean, mean),
      new BuySellTraderFactory(turn, turn),
      new BuySellTraderFactory(turn, stubborn)
      //new BuySellTraderFactory(stubborn, turn)
      //new BuySellTraderFactory(mean, mean),
      //new BuySellTraderFactory(mean, stubborn)
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

  def printDetails(t: Trader): Unit = {
    println(s"Detailed information about $t:")
    println(s"\tWent to ${t.m} ${t.nTradesTried} times")
    println(s"\tSold ${t.nSells} times. Bought ${t.nBuys} times.")
    if (t.cashLostToRounding > 0.0) {
      println(s"\t[Warning] Due to rounding, ${t.name} lost" +
        f" ${t.cashLostToRounding}%.2f" +
        s" of ${t.currency}.")
    }
    if (t.btcLostToRounding > 0.0) {
      println(s"\t[Warning] Due to rounding, ${t.name} lost" +
        f"${t.btcLostToRounding}%.2f" +
         " of BTC.")
    }
  }

  def printReturns(t: Trader, r: Double): Unit =
    println(s"${t.name} returns at ${t.m}: $r")

  /* paramSelect uses heuristic algorithms to figure out the best parameters
   * for selected traders. */
  def paramSelect(
      initSoln: Params,
      neighborOf: Params => Params,
      newTrader: Params => Trader): Params = {
    def costOf(param: Params): Double = {
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

    val SA = new Annealing[Params](
        costOf _,
        neighborOf,
        initSoln,
        initTemp,
        alpha,
        maxTime)
    SA.run()
    SA.bestSoln
  }

  type Params = (Int, Double, Double, Double)
  //type Params = (Int, Double, Double)

  def heuristicMain(): Unit = {
    val hcbm2 = new HistoricalMarket(new CoinbaseMarket(2))
    hcbm2.open()

    def neighborOf(param: Params): Params = {
      val p1 = param._1 + stepSize*(nextInt(3) - 1)
      val p2 = param._2 + stepSize*(nextDouble - 0.5)
      val p3 = param._3 + stepSize*(nextDouble - 0.5)
      val p4 = param._4 + stepSize*(nextDouble - 0.5)
      (if (p1 < 0) 0 else p1,
        if (p2 < 0) 0 else p2,
        if (p3 < 0) 0 else p3,
        if (p4 < 0) 0 else p4)
    }

    def newTrader(ps: Params): Trader = {
      //val s = (TurnTraderFactory.apply _).tupled(ps)
      val s = (TurnTraderFactory.apply _).tupled(ps)
      val b = (TurnTraderFactory.apply _).tupled(ps)
      val f = new BuySellTraderFactory(s, b)
      traderFromFactory(f, hcbm2)
    }

    val initSoln = turnParams

    val params = paramSelect(initSoln, neighborOf, newTrader)
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
    //traders foreach printDetails
    //traders foreach Plotter.plotTraderHistory
    //traders.head.history foreach println
  }

  def coinBaseMain(): Unit = {
    /*
    // Testing things
    val cbm = new CoinbaseMarket()
    cbm.open()
    println(cbm.quoteToSell(1.0, currency))
    println(cbm.quoteToBuy(1.0, currency))
    cbm.tradeHistory foreach println
    val h = cbm.history
    val dt = h.head.time - h.tail.head.time
    println(s"dt = $dt")
    */

    var page = 0
    while (true) {
      page += 1
      val hcbm = new HistoricalMarket(new CoinbaseMarket(page))
      hcbm.open()
      val t = getRanTraders(
      /*
          () => List(new TurnTrader(
            hcbm,
            capital,
            initBTCs,
            currency,
            windowSize,
            minRisingSlope,
            maxDroppingSlope,
            minTurnChange))
          */
          () => List(traderFromFactory(RandomTraderFactory, hcbm))
        ).head
      printReturns(t, t.returns)
    }
    //Plotter.plotTraderHistory(trader)
  }

  // What to run to just trade and try to make money.
  def makeMoneyMain(): Unit = {
    val m = RandomMarket
    val f = new BuySellTraderFactory(random, random)
    val t = traderFromFactory(f, m)

    m.open()
    while (true) {
      m.update()
      t.tryToTrade()
      println(s"Time $time")
      println(s"\tPrice:${m.spotPrice}")
      println(s"\tReturns:${t.returns}")
      Thread sleep minDTime
    }
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
