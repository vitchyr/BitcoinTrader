import java.util.Date
import scala.util.Random.nextFloat

object RandomFakeMarket extends FakeMarket {
  private var price: Float = 100f
  def iterator = new Iterator[Float] {
    def hasNext = true
    def next = { val old_price = price; price += nextFloat(); old_price }
  }
}
