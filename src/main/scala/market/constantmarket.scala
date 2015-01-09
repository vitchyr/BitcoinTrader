package market {
  // A market whose price is constant.
  object ConstantMarket extends FakeMarket {
    def iterator = new Iterator[Double] {
      def hasNext = true
      def next = 10.0
    }

    def resetState(): Unit = ()

    override def toString = "Constant Market"
  }
}
