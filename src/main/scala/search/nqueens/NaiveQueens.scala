package search.nqueens

object NaiveQueens {
  def queen(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else {
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
      }
    }

    def isSafe(col: Int, l: List[Int]): Boolean = {
      val row = l.length
      val positions = (0 until row) zip l
      positions forall {
        case (r, c) => c != col && row - r != Math.abs(col - c) // slope != 1
      }
    }
    placeQueens(n)
  }
}
