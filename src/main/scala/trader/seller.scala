import market.Market
import defs._

package trader {
  // Someone that determines how much to sell.
  trait Seller {
    /* Update the seller, probably because the market was updated. */
    def update(): Unit

    /* Get the amount of bitcoins to sell. */
    def amountToSell: Double

    /* Update the state after the trader has sold bitcoins. */
    def updateAfterSell(trans: Transaction): Unit

    /* Update the state after the trader has bought bitcoins. */
    def updateAfterBuy(trans: Transaction): Unit

    /* The name of this type of seller. */
    def name: String
  }

  trait SellerFactory {
    def newSeller(
        m: Market,
        cash: Double,
        btc: Double,
        currency: String): Seller
  }
}
