package market {
  // A market whose price follows a math function
  trait MathMarket extends FakeMarket {
    private var t: Double = 0
    private val dt = 0.05

    def shapeFunction(time: Double): Double

    def iterator = new Iterator[Double] {
      def hasNext = true
      def next = {
        val old_price = shapeFunction(t)
        t += dt
        old_price
      }
    }

    def resetState(): Unit = t = 0

    override def toString = "SinusoidMarket"
  }
}
