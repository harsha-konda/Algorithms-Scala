package sort

object InsertionSort extends App {
  def insert(x: Int, l: List[Int]): List[Int] = l match {
    case List() => List(x)
    case h :: tail => if (x < h) x :: l else h :: insert(x, tail)
  }

  def sort(l: List[Int]): List[Int] = {
    def isort(x: List[Int], accum: List[Int]): List[Int] = x match {
      case List() => accum
      case x :: tail => isort(tail, insert(x, accum))
    }

    isort(l, List())
  }

  print(sort(List(5, 6, 4, 1, 9, 8)))
}
