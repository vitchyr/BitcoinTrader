import defs._
import utils._

package market { 
  // Recreate a market from the history of another market
  class HistoricalMarket(m: Market) extends FakeMarket {
    private var i = 0
    private val h = m.history.toArray
    println(h.length)

    def iterator = new Iterator[Double] {
      def hasNext = i < h.length
      def next = {
        val n = h(i)
        i += 1
        n.price
      }
    }

    def resetState(): Unit = i = 0

    override def toString = s"Historical '$m' Market"
  }
}
