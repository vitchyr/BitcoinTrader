import util.Random.nextDouble

package market {
  // A market whose price goes up and down according to a sinusoid + noise
  class NoisyMarket(fm: FakeMarket) extends FakeMarket {
    private var t: Double = 0
    private val dt = 0.01
    private val A = 50 // amplitude
    private val b = 100 // offset
    private val An = 5 // amplitude of noise

    def iterator = new Iterator[Double] {
      def hasNext = fm.iterator.hasNext
      def next = fm.iterator.next + (nextDouble - 0.5) * An
    }

    override def toString = s"Noisy '$fm' Market"
  }

  object NoisyMarket {
    def apply (fm: FakeMarket) = new NoisyMarket(fm)
  }
}
