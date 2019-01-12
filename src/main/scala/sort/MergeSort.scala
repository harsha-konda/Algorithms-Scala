package sort

object MergeSort extends App {
  def combine(a: List[Int], b: List[Int]): List[Int] = {
    def combine(a: List[Int], b: List[Int], accum: List[Int]): List[Int] = {
      a match {
        case List() => accum ++ b
        case head :: tail => {
          if (b.isEmpty)
            accum ++ a
          else if (head > b.head) {
            combine(tail, b, accum ++ List(head))
          }
          else
            combine(a, b.tail, accum ++ List(b.head))
        }
      }
    }
    combine(a, b, List())
  }

  def sort(l: List[Int]): List[Int] = l match {
    case List() => l
    case head :: List() => l
    case a :: b :: List() => if (a > b) l else b :: a :: List()
    case default => {
      val middle = l.size / 2
      val a = l.splitAt(middle)
      combine(sort(a._1), sort(a._2))
    }
  }

}
