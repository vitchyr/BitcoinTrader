import defs._
import scala.util.Random.nextDouble
import scala.io.Source

package market { 
  object CoinDeskMarket extends VirtualMarket {
    val dataFileName: String = "data/coindesk-bpi-USD-close_data-2013-12-21" +
      "_2014-12-21.csv"

    // Given a csv two two columns (first is date, second is price)
    def getCoinDeskDataFrom(fname: String): Array[Double] = {
      val src = Source.fromFile(fname)
      val iter = src.getLines().drop(1) // drop the header
      val data = iter map (_.split(",")(1).toDouble)
      src.close()
      data.toArray
    }

    private var dataIdx: Int = 0 // index into data where we currently are.
    private val data: Array[Double] = getCoinDeskDataFrom(dataFileName)

    def iterator = new Iterator[Double] {
      def hasNext = true
      def next = {
        dataIdx += 1
        100
      }
    }

    override def toString = "CoinDesk Market"
  }
}
