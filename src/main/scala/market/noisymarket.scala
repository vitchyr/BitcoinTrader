import util.Random.nextDouble

package market {
  // A market whose price goes up and down according to a sinusoid + noise
  class NoisyMarket(fm: FakeMarket) extends FakeMarket {
    private val An = 3 // amplitude of noise

    def iterator = new Iterator[Double] {
      def hasNext = fm.iterator.hasNext
      def next = fm.iterator.next + (nextDouble - 0.5) * An
    }

    def resetState(): Unit = ()

    override def toString = s"Noisy '$fm' Market"
  }

  object NoisyMarket {
    def apply (fm: FakeMarket) = new NoisyMarket(fm)
  }
}
