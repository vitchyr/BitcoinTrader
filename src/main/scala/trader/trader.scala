import market.Market

package trader {
  trait Trader { 
    var cash: Double;
    var bitcoins: Double;
    val m: Market;
    val currency: String;

    def getSellRate(): Double = m.getSellInfo(currency)._1
    def getBuyRate(): Double = m.getBuyInfo(currency)._1

    def buy(amount: Double): Unit = {
      val buyRate = getBuyRate()
      if (amount * buyRate > cash) {
        //println(s"Don't have enough cash to buy $amount bitcoins.")
        return ()
      }
      val trans = m.buy(amount, currency)
      bitcoins += trans.dBitcoin
      cash -= trans.dCash
    }

    def sell(amount: Double): Unit = {
      if (amount > bitcoins) {
        //println(s"Don't have $amount bitcoins to sell.")
        return ()
      }
      val trans = m.sell(amount, currency)
      bitcoins -= trans.dBitcoin
      cash += trans.dCash
    }
    
    // Tell the trader to try trading for one iteration.
    def trade(): Unit
    // Returns the amount of money left (capital not invested + money made)
    def getMoneyLeft(): Double = cash + getSellRate() * bitcoins;
  }
}
