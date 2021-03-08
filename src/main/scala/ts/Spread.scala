
package ts

// type maybeNestedList[T] = List[T | List[T]]
type maybeNestedList[T] = List[T | List[T]]

extension listOps on [T](nt: maybeNestedList[T]) {
  def spread: List[T] = nt flatten {
      case i: List[T] => i.spread
      case e: T => List(e)
    }
}



