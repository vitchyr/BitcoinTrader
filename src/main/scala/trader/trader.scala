import market.Market
import defs._

package trader {
  trait Trader { 
    var cash: Double;
    var bitcoins: Double;
    val m: Market;
    val currency: String;

    /*************************
     ** Method to implement **
     *************************/
    // Update the trader so that they know if they want to buy or sell
    def update(): Unit
    // Amount of bitcoins to sell
    def amountToSell: Double
    // Amount of bitcoins to buy
    def amountToBuy: Double

    /*************************
     ** Implemented Methods **
     *************************/
    def sellInfo: BitcoinInfo = m.getSellInfo(currency)
    def buyInfo: BitcoinInfo = m.getBuyInfo(currency)
    def sellRate: Double = sellInfo.price
    def buyRate: Double = buyInfo.price

    def buy(amount: Double): Unit = {
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
    def trade(): Unit = {
      update()
      sell(amountToSell)
      buy(amountToBuy)
    }

    // Returns the amount of money left (capital not invested + money made)
    def moneyLeft: Double = cash + sellRate * bitcoins;
    def isBroke: Boolean = cash == 0.0 && bitcoins == 0.0
  }

  trait TraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): Trader
  }
}
