package mathutil {
  object Regression{
    def sum(xs: List[Double]): Double = (0.0 /: xs)(_+_)

    def mean(data: List[Double]): Double = sum(data) / data.length

    // Returns (a, b), where y = bx + a is the line of best fit
    def linFit(ys: List[Double], xs: List[Double]): Tuple2[Double, Double] = {
      if (xs.length != ys.length) {
        sys.error("linFit: xs and ys must be the same length")
      }
      val N = xs.length
      val xSum = sum(xs)
      val ySum = sum(ys)
      //Slope(b) = (NΣXY - (ΣX)(ΣY)) / (NΣX^2 - (ΣX)^2)
      //Intercept(a) = (ΣY - b(ΣX)) / N
      val b = ((N * sum((xs zip ys) map { case(x, y) => x*y }) - xSum * ySum)
        / (N * sum(xs map (x => x*x)) - xSum * xSum))
      val a = (ySum - b * xSum) / N
      (a, b)
    }

    // Do a linear fit, assuming that the xs are linearly spaces by 1
    // Returns (a, b), where y = bx + 1
    def linSpaceFit(ys: List[Double]): Tuple2[Double, Double] = {
      linFit(ys, (List.range(0, ys.length) map (x => x.toDouble)))
    }
  }
}
