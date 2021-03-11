package co.s4n.conductor

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class conductorSpec extends AnyFlatSpec with Matchers{
  "Escuderia" should "Carlos" in {
    new Escuderia(nombre = "Carlos", new Conductor("juan","cardona",7,4)).nombre shouldEqual "Carlos" }


}
