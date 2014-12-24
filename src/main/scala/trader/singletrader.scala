import market.Market
import defs._

package trader {
  /* A single trader with one strategy
   *
   * The following distinction is made between value and price:
   *    value = how much you can sell something for
   *    price = how much it costs to buy something.
   */
  trait SingleTrader extends Trader { 
    var cash: Double;
    var bitcoins: Double;
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

    // Update the trader after having sold this amount
    def updateAfterSell(trans: Transaction): Unit

    // Update the trader after having bought this amount
    def updateAfterBuy(trans: Transaction): Unit
    
    /*************************
     ** Implemented Methods **
     *************************/
    // The next 4 methods get a quote for a transaction. The amount bought/sold
    // is in BTC or cash.
    def sellQuote(amount: Double): Transaction = m.quoteToSell(amount, currency)
    def sellQuoteCash(amount: Double): Transaction =
      m.quoteToSellCash(amount, currency)
    def buyQuote(amount: Double): Transaction = m.quoteToBuy(amount, currency)
    def buyQuoteCash(amount: Double): Transaction =
      m.quoteToBuyCash(amount, currency)

    // Returns the amount of money made if [amount] BTCs are sold
    def valueOf(amount: Double): Double = sellQuote(amount).dCash

    // Returns the value of 1 BTC
    def btcValue: Double = valueOf(1.0)

    // Returns the amount of money needed to buy [amount] BTCs
    def priceOf(amount: Double): Double = -buyQuote(amount).dCash

    // Returns the price of 1 BTC
    def btcPrice: Double = priceOf(1.0)

    // Returns how many BTCs you would have to sell to get [amount] cash
    def bitcoinsCanSellWith(amount: Double): Double =
      -m.quoteToSellCash(amount, currency).dBitcoins

    // Returns how many BTCs you can buy with [amount] cash
    def bitcoinsCanBuyWith(amount: Double): Double =
      m.quoteToBuyCash(amount, currency).dBitcoins

    // Returns the maximum amount of BTCs this trader can buy.
    def maxBTCsCanBuy: Double = bitcoinsCanBuyWith(cash)

    // Update this trader's cash and bitcoins based on a transaction.
    def updateBTCsAndCash(trans: Transaction): Unit = {
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
      //println(s"** UPDATE $this: BTC = $bitcoins. cash = $cash")
    }

    // Sell [amount] of bitcoins at the market.
    def sell(amount: Double): Unit = {
      if (amount <= 0 || amount > bitcoins) {
        if (amount < 0) {
          sys.error(s"Can't sell $amount BTCs - $this")
        }
        return ()
      }
      val trans = m.sell(amount, currency)
      updateBTCsAndCash(trans)
      updateAfterSell(trans)
    }

    // Buy [amount] of bitcoins at the market.
    def buy(amount: Double): Unit = {
      if (amount <= 0 || priceOf(amount) > cash) {
        if (amount < 0) {
          sys.error(s"Can't buy $amount BTCs - $this")
        }
        return ()
      }
      val trans = m.buy(amount, currency)
      updateBTCsAndCash(trans)
      updateAfterBuy(trans)
    }

    // Tell the trader to try trading for one iteration.
    def trade(): Unit = {
      update()
      val toSell = amountToSell
      val toBuy = amountToBuy
      // It's important to get the values before because sell and buy have
      // side effects
      sell(toSell)
      buy(toBuy)
    }

    // Returns the amount of money left (capital not invested + money made)
    def moneyLeft: Double = {
      //println(s"cash = $cash. bitcoins = $bitcoins")
      cash + valueOf(bitcoins)
    }
  }

  trait SingleTraderFactory extends TraderFactory {
    def newTrader(m: Market, cash: Double, currency: String): SingleTrader
  }
}
