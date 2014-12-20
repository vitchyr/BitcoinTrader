import java.util.Date

trait Market {
  // Buy [amount] of bitcoints. Returns the amount sucessfully bought.
  def buy(amount: Float): Float
  // Sell [amount] of bitcoints. Returns the amount sucessfully sold.
  def sell(amount: Float): Float
  // Get the exchange rate. Returns how much 1 bitcoin would cost in [currency]
  def getRate(currency: String): Tuple2[Float, Date]
}
