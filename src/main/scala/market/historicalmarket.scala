import defs._
import utils._

package market { 
  // Recreate a market from the history of another market
  class HistoricalMarket(m: Market) extends FakeMarket {
    private var i: Int = 0
    private var t: Long = 0

    private lazy val h = {
      m.history.toArray
    }

    override def iterTime: Long = t

    def iterator = new Iterator[Double] {
      def hasNext = i < h.length
      def next = {
        val n = h(i)
        t = n.time
        i += 1
        n.price
      }
    }

    def resetState(): Unit = {
      i = 0
      t = 0
    }

    override def open(): Unit = m.open()

    override def toString = s"Historical '$m' Market"
  }
}
