class Parent[@specialized(Int) T]

object Test extends App {

  def getParentName[@specialized(Int) T](t: T) = {
    class Clazz extends Parent[T] { 
      def parentName = this.getClass.getSuperclass.getSimpleName.toString
    }
    (new Clazz).parentName
  }

  def assertEqual(o1: Any, o2: Any) = 
    assert(o1 == o2, "Expected " + o1 + " to be equal to " + o2 + ".")

  // We need to make sure the specialization phase transforms
  // Clazz's parent from Parent[Int] to Parent$mcI$sp
  assertEqual(getParentName("X"), "Parent")
  assertEqual(getParentName(123), "Parent$mcI$sp")
}
