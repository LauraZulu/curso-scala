package co.s4n.traits
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class traits extends AnyFlatSpec with Matchers{
  "Circulo" should "Un circulo de radio radio 2" in {
    Draw.apply2(new Circulo(2)) shouldEqual "Un circulo de radio 2.0" }
  "El perimetro de rectangulo(2,3)" should "10.0" in {
    (new Rectangulo(2,3).perimetro)  shouldEqual 10.0 }
  "El area de cuadrado(2)" should "4.0" in {
    (new Cuadrado(2).area)  shouldEqual 4.0 }
  "Nuevo color, atributo red" should "255" in {
    Color(255,0,230).red  shouldEqual 255 }
  "Tamaño de melena de un nuevo Leon" should "Grande" in {
    new Leon().tamañoDeMelena shouldEqual "Grande" }
}

