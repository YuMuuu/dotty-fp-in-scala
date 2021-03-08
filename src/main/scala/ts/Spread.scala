
package ts

type maybeNestedList[T] = List[T | List[T]]
extension listOps on [T](nt: maybeNestedList[T]) {
  def spread: List[T] = nt flatten {
      case i: List[T] => spread 
      case e: T => List(e)
    }
}

val l: List[Int] = (List(1, 2, List(3, 4)): maybeNestedList[Int]).spread




