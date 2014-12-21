// Define definitions that everyone will use

package defs {
  // Holds how many bitcoins changes for how much cash in [currency]
  class Transaction(val dBitcoin: Double, val dCash: Double,
      val currency: String)

  // Holds statistics about bitcoins at some point in time
  class BitcoinInfo(val price: Double, val time: Double, val currency: String)
}
