import scala.util.Random.nextDouble
import math.exp

package heuristics {
  /* Performs simulated annealing */
  class Annealing[A](
      costOf: A => Double, // lower cost is better
      randNeighborOf: A => A,
      initSoln: A,
      initTemp: Double,
      alpha: Double,
      maxTime: Int) {

    var _bestSoln = initSoln
    var bestCost = costOf(initSoln)
    var currSoln = initSoln
    var t = initTemp

    // Run the simulated annealing.
    def run(): Unit = {
      1 to maxTime foreach (_ => {
        val nextSoln = randNeighborOf(currSoln)
        val nextCost = costOf(nextSoln)
        //println(s"New soln of $nextSoln has a cost of $nextCost")

        if (nextCost < bestCost) {
          _bestSoln = nextSoln
          bestCost = nextCost
          currSoln = nextSoln
        } else {
          val dCost = nextCost - bestCost
          if (nextDouble <= exp(-dCost / t)) {
            currSoln = nextSoln
          }
        }
        t *= alpha
      })
    }
    
    def bestSoln: A = _bestSoln
  }
}
