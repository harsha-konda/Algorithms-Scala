package sort

object MergeSort extends App {
  def combine(a: List[Int], b: List[Int]): List[Int] = {
    (a, b) match {
      case (_, List()) => a
      case (List(), _) => b
      case (aHead :: aTail, bHead :: bTail) => {
        if (aHead < bHead) aHead :: combine(aTail, b)
        else bHead :: combine(a, bTail)
      }
    }
  }

  def sort(l: List[Int]): List[Int] = {
    val middle = l.size / 2
    if (middle == 0) l
    else {
      val (left, right) = l.splitAt(middle)
      combine(sort(left), sort(right))
    }

  }

}
