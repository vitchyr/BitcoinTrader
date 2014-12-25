import defs._
import scalax.chart.module.Charting._
import breeze.linalg._
import breeze.plot._
import trader._

package plotter {
  object Plotter {
    def plotTraderHistory(t: Trader): Unit = {
      val fname =
        s"plots/${t}_${t.m.toString.replaceAll(" ", "_")}.png"
      val (mTs, mPrices) = (for (h <- t.m.history) yield (h.time, h.buyRate))
        .unzip
      //val chart = XYLineChart(mData)
      //chart.saveAsPNG(fname)
      val tData = for (h <- t.history) yield (h.time, h.dBitcoins, h.dCash)
      val (buyData, sellData) = tData partition { case(_, dB, _) => dB > 0 }
      val (buyTs, buyPrices) = (buyData map
        { case (t, dB, dC) => (t.toDouble, -dC/ dB) }).unzip
      val (sellTs, sellPrices) = (sellData map
        { case (t, dB, dC) => (t.toDouble, -dC/ dB) }).unzip


      //(buyTs zip sellTs) foreach (x => println(x))
      (buyTs zip sellTs) foreach println

      val f = Figure()
      val p = f.subplot(0)
      p += plot(mTs, mPrices, '-')
      p += plot(buyTs, buyPrices, '+')
      p += plot(sellTs, sellPrices, '.')
      p.xlabel = "Time"
      p.ylabel = "Price ($ / BTC)"
      f.saveas(fname)
    }
  }
}
