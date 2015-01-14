import market.Market
import defs._

package trader {
  // Someone that determines how much to buy.
  trait Buyer {
    /* Update the seller, probably because the market was updated. */
    def update(): Unit

    /* Get the amount of bitcoins to buy. */
    def amountToBuy: Double

    /* Update the state after the trader has sold bitcoins. */
    def updateAfterSell(trans: Transaction): Unit

    /* Update the state after the trader has bought bitcoins. */
    def updateAfterBuy(trans: Transaction): Unit

    /* The name of this type of seller. */
    def name: String
  }

  trait BuyerFactory {
    def newBuyer(
        m: Market,
        cash: Double,
        btc: Double,
        currency: String): Buyer
  }
}
