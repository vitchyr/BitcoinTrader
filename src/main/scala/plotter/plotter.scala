import defs._
import breeze.plot._
import trader._

package plotter {
  object Plotter {
    def plotTraderHistory(t: Trader): Unit = {
      val fname =
        s"plots/${t.name}_at_${t.m.toString}.png".replaceAll(" ", "_")

      val (mTs, mPrices) = (for (h <- t.m.history) yield (h.time, h.buyRate))
        .unzip

      val tData = for (h <- t.history) yield (h.time, h.dBitcoins, h.dCash)
      val (buyData, sellData) = tData partition { case(_, dB, _) => dB > 0 }
      val (buyTs, buyPrices) = (buyData map
        { case (t, dB, dC) => (t.toDouble, -dC/ dB) }).unzip
      val (sellTs, sellPrices) = (sellData map
        { case (t, dB, dC) => (t.toDouble, -dC/ dB) }).unzip

      val f = Figure()
      val p = f.subplot(0)
      p += plot(mTs, mPrices, '-')
      p += plot(buyTs, buyPrices, '+')
      p += plot(sellTs, sellPrices, '.')
      p.xlabel = "Time (+ = buy, - = sell)"
      p.ylabel = "Price ($ / BTC)"
      p.title = s"Transactions of ${t.name} vs. Time" + 
        f"\n(returns = ${t.returns}%3.2f%%)"
      f.saveas(fname)
    }
  }
}
