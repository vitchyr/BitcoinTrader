import math.cos
import util.Random.nextFloat

package market {
  // A market whose price goes up and down according to a sinusoid + noise
  object SinusoidNoiseMarket extends FakeMarket {
    private var t: Double = 0
    val dt = 0.01
    val A = 50 // amplitude
    val b = 100 // offset
    val An = 5 // amplitude of noise

    def iterator = new Iterator[Double] {
      def hasNext = true
      def next = {
        val old_price = b + A*cos(t) - nextFloat + 0.5
        t += dt
        old_price
      }
    }

    override def toString = "SinusoidNoiseMarket"
  }
}
