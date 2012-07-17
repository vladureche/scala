import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.comment._
import scala.tools.partest.ScaladocModelTest

// SI-5079 "Scaladoc can't link to an object (only a class or trait)"
// SI-4497 "Links in ScalaDoc - Spec and implementation unsufficient"
// SI-4224 "Wiki-links should support method targets"
// SI-3695 "support non-fully-qualified type links in scaladoc comments"
object Test extends ScaladocModelTest {

  override def resourceFile = "external-links.scala"
  def ctLinks(c: Comment) = {
      def countLinks(body: Any): Int = body match {
        case el: EntityLink => 1
        case s: Seq[_]  => s.toList.map(countLinks(_)).sum
        case p: Product => p.productIterator.toList.map(countLinks(_)).sum
        case _          => 0
      }
      countLinks(c.body)
    }

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // just need to check the member exists, access methods will throw an error if there's a problem
    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("externallinks")
    val TEST = base._object("TEST")

    val links = ctLinks(TEST.comment.get)
    assert(links == 2, links +   " == 2 (the member links in object TEST)")
  }
}
