import math.sin

package market {
  // A market whose price goes up and down according to a sinusoid.
  object CosMarket extends MathMarket {
    private val A = 5 // amplitude
    private val b = 100 // offset

    def shapeFunction(t: Double): Double = b + A*sin(t)

    override def toString = "Cosine Market"
  }
}
