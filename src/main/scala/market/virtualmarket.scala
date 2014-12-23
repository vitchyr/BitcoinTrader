import defs._

package market { 
  /* Markets use real data for the buy/sell prices, but don't actually buy/sell
   * from those markets. */
  trait VirtualMarket extends FakeMarket {
    val dataFileName: String
  }
}
