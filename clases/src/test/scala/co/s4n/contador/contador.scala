package co.s4n.contador

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class contador extends AnyFlatSpec with Matchers{
  "Contador" should "20" in {
    new Contador(20).decr().incr().decr().incr().contador() shouldEqual 20 }
  "Contador con ajuste" should "23" in {
    new Contador(20).ajuste(new Sumador(3)).contador() shouldEqual 23 }

}
