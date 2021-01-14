// 1
class Container[A](private val value: A) {
  def getContent: A = {
    value
  }

  def apply[R](function: A => R): R = {
    function.apply(value)
  }
}

// 2 + 3
trait Maybe[+A] {
  def applyFunction[R](function: A => R): Maybe[R]
  def flatMap[R](function: A => Maybe[R]): Maybe[R]
  def getOrElse[R >: A](default: R): R
}

object No extends Maybe[Nothing] {
  override def applyFunction[R](function: Nothing => R): Maybe[R] = this
  override def flatMap[R](function: Nothing => Maybe[R]): Maybe[R] = this
  override def getOrElse[R >: Nothing](default: R): R = default
}

class Yes[A](private val value: A) extends Maybe[A] {
  override def applyFunction[R](function: A => R): Maybe[R] = new Yes(function(value))
  override def flatMap[R](function: A => Maybe[R]): Maybe[R] = function(value)
  override def getOrElse[R >: A](default: R): R = value
}

//

object Main {
  def main(args: Array[String]): Unit = {
    // nbd9
    println("<---- 1 ---->")

    def reverse(str: String): String = str.reverse

    val container = new Container("sedes, but longer to test reverse")
    println(container.getContent())
    println(container.apply(reverse))

    println("<----2 3---->")

    val nope = No
    val yep1 = new Yes(42)
    val yep2 = new Yes("🤔🤔")

    println(nope.isInstanceOf[Maybe[Nothing]])
    println(yep1.isInstanceOf[Maybe[Int]])
    println(yep2.isInstanceOf[Maybe[String]])
    println(nope.isInstanceOf[Maybe[_]])
    println(yep1.isInstanceOf[Maybe[_]])
    println(yep2.isInstanceOf[Maybe[_]])

    println("<----3 4---->")

    def subtractTwo(num: Int): Int = {
      var rtrn = num
      rtrn -= 2
      rtrn
    }

    println(yep1.getOrElse(0))
    println(yep2.getOrElse("yes"))
    println(nope.getOrElse("no"))

    val one = yep1.applyFunction(subtractTwo)
    val two = nope.applyFunction(subtractTwo)
    println(one.getOrElse("no one"))
    println(two.getOrElse[String]("no too"))
    val three = one.asInstanceOf[Maybe[Int]]
    println(three.getOrElse("no 3"))

    // nbd10
    def subButWrapped(obj: Maybe[Int]): Maybe[Int] = {
      obj.applyFunction(subtractTwo)
    }

    println("<---- 2 ----> (but nbd10)")
    val obj1 = new Yes(356)
    val obj2 = new Yes(obj1)

    val result1 = obj1.applyFunction(subButWrapped)
    assert(result1.getClass.equals(obj1.getClass))
    assert(result1.getOrElse("no value").getClass.equals(obj1.getClass))
    assert(result1.getOrElse("no value").asInstanceOf[Maybe[Int]].getOrElse("no value").getClass
      .equals("".getClass))

    val result2 = obj2.flatMap(subButWrapped)
    assert(result2.getClass.equals(obj2.getClass))
    assert(result2.getOrElse("no value").getClass.equals("".getClass))
  }
}
