package ts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams
import ts.{listOps, maybeNestedList}
import scala.language.implicitConversions
    
class SpreadSpec extends AnyFlatSpec with Diagrams {
  it should "nestの深さが違うlistをfllatenできる" in {
    assert((List(1, 2, List(3, 4)): maybeNestedList[Int]).spread === List(1, 2, 3, 4))
    //再帰的定義はできていないのでこれは通らない
    // assert((List(1, 2, List(3, 4), List(5, List(6, 7))): maybeNestedList[Int]).spread === List(1, 2, 3, 4)) 
  }
}

