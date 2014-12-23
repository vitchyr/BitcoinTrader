import scala.util.Random.nextDouble

package market {
  // A market whose price randomly changes. It can even be negative!
  object RandomMarket extends FakeMarket {
    private var price: Double = 100

    def iterator = new Iterator[Double] {
      def hasNext = true
      def next = {
        val old_price = price
        price += nextDouble() - 0.5
        old_price
      }
    }

    override def toString = "Random Market"
  }
}
