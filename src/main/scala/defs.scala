// Define definitions that everyone will use

package defs {
  /* Holds how many bitcoins were sold/bought for how much cash in [currency]
   * at a given time.
   *     dB = change in bitcoin
   *     dC = change in Cash
   */
  class Transaction(dB: Double, dC: Double,
      val time: Long, val currency: String) {
    val dBitcoins = Transaction.roundBtc(dB)
    val dCash = Transaction.roundCash(dC)
  }

  object Transaction {
    val CashPrecision: Int = 2 // 2 decimal place. Rounds to the cent
    val BtcPrecision: Int = 4 // 4 decimal place.

    def round(d: Double, precision: Int): Double =
      BigDecimal(d).setScale(precision, BigDecimal.RoundingMode.FLOOR)
        .toDouble

    def roundBtc(x: Double): Double = round(x, BtcPrecision)

    def roundCash(x: Double): Double = round(x, CashPrecision)
  }
}
