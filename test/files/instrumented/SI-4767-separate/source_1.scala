package instrumented

trait A {
  @inline final def foo() = println("hello from A")  
}
