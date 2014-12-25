import scala.collection.mutable.ArrayBuffer

// Definitions that everyone will use
package object defs {
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

  class BitcoinStat(val time: Double, val buyRate: Double)

  type TraderHistory = List[Transaction]
  type MarketHistory = List[BitcoinStat]

  // TODO: Use this
  class BankAccount(var cash: Double, var bitcoins: Double) {
    private var _historyLog: ArrayBuffer[Transaction] = new ArrayBuffer()

    def historyLog: List[Transaction] = _historyLog.toList

    def updateBank(trans: Transaction): Unit = {
      bitcoins += trans.dBitcoins
      cash += trans.dCash
      bitcoins = Transaction.roundBtc(bitcoins)
      cash = Transaction.roundCash(cash)
      if (bitcoins < 0) {
        sys.error(s"Can't have $bitcoins BTCs")
      }
      if (cash < 0) {
        sys.error(s"Can't have $cash cash")
      }
      _historyLog append trans
      //println(s"** UPDATE $this: BTC = $bitcoins. cash = $cash")
    }
  }
}
