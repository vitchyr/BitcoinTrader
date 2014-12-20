object MoneyMaker {
  def main(args: Array[String]) {
    for (i <- List.range(0, 10)) {
      println(RandomFakeMarket.getRate())
    }
  }
}
