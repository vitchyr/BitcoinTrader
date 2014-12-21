import math.cos

package market {
  // A market whose price goes up and down according to a sinusoid.
  object SinusoidMarket extends FakeMarket {
    private var t: Double = 0
    val dt = 0.01
    val A = 50 // amplitude
    val b = 100 // offset

    def iterator = new Iterator[Double] {
      def hasNext = true
      def next = {
        val old_price = b + A*cos(t)
        t += dt
        old_price
      }
    }

    override def toString = "SinusoidMarket"
  }
}
