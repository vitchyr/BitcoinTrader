import java.util.Date
import java.util.Calendar

trait FakeMarket extends Market with Iterable[Float] {
  lazy val calendar = Calendar.getInstance()
  def time(): Date = calendar.getTime()

  def buy(amount: Float): Float = amount;
  def sell(amount: Float): Float = amount;
  def getRate(currency: String): Tuple2[Float, Date] = {
    (this.iterator.next(), time())
  }
}
