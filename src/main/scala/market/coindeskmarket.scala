import defs._
import scala.util.Random.nextDouble

package market { 
  object CoinDeskMarket extends VirtualMarket {
    val dataFileName: String = "data/coindesk-bpi-USD-close_data-2013-12-21" +
      "_2014-12-21.csv"
    private var dataIdx: Int = 0 // index into data where we currently are.

    def iterator = new Iterator[Double] {
      def hasNext = true
      def next = {
        100
        dataIdx += 1
      }
    }

    override def toString = "CoinDesk Market"
  }
}
