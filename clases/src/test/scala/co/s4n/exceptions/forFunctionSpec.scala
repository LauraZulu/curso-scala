package co.s4n.exceptions

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class forFunctionSpec extends AnyFlatSpec with Matchers{
  "Map con for" should "List(2, 4, 6, 8)" in {
    forFunctions.mapa(List(1,2,3,4))(x => x*2) shouldEqual List(2, 4, 6, 8)}
  "Filtro con for" should "List(4, 5)" in {
    forFunctions.filtro(List(1,2,3,4,5))(_>=4) shouldEqual List(4, 5)}
  "El número 7 es primo" should "true" in {
    forFunctions.esPrimo(7) shouldEqual true}
  "Calculo de la longitud con for" should "4" in {
    forFunctions.miLongitud(List(1,2,3,4)) shouldEqual 4}
  "Ultimo elemento de la lista" should "9" in {
    forFunctions.myLast1(List(1,2,3,4,9)) shouldEqual 9}
  "El elemento 2 de la lista" should "4" in {
    forFunctions.elementAt(2,List(2,3,4,5,6)) shouldEqual 4}
  "La copia de la lista" should "List(1,2,3,4,5)" in {
    forFunctions.copy(List(1,2,3,4,5)) shouldEqual List(1,2,3,4,5)}
  "El reverso de la lista" should "List(5, 4, 3, 2, 1)" in {
    forFunctions.reverse(List(1,2,3,4,5)) shouldEqual List(5, 4, 3, 2, 1)}

}
