import defs._
import scala.util.Random.nextDouble
import scala.io.Source
import java.io.{FileReader, FileNotFoundException, IOException}

package market { 
  /* A market based on the bitcoin data from the past 366 days provided by
   * CoinDesk, as of mid-December 2014
   *
   * nDrop and nDropFromEnd are used to look at only certain time periods.
   * [nDrop] how many days to drop from the front.
   * [nDropFromEnd] how many days to drop from the end.
   */
  class CoinDeskMarket(
      nDrop: Int = 0,
      nDropFromEnd: Int = 0)
    extends VirtualMarket {
    val dataFileName: String = "data/"+
      "coindesk-bpi-USD-close_data-2013-12-21_2014-12-21.csv"

    // Given a csv two two columns (first is date, second is price)
    def getCoinDeskDataFrom(fname: String): Array[Double] = {
      val src = Source.fromFile(fname)
      val iter = src.getLines().drop(1) // drop the header
      val data = iter map (_.split(",")(1).toDouble)
      val out = data.toArray
      src.close()
      out.drop(nDrop)
      if (nDropFromEnd > 0) out.reverse.drop(nDropFromEnd).reverse else out
    }

    private var i: Int = 0 // index into data where we currently are.
    private val data: Array[Double] = getCoinDeskDataFrom(dataFileName)

    def iterator = new Iterator[Double] {
      def hasNext = i < data.length
      def next = {
        i += 1
        data(i - 1)
      }
    }

    def resetState(): Unit = i = 0

    override def toString = "CoinDesk Market"
  }
}
