package co.s4n.inmutable.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class List_test extends AnyFlatSpec with Matchers{
  "El head de la lista" should "1" in {
    List.head(lst = List(1,2,3,4)) shouldEqual 1}
  "El maximo de la lista" should "4" in {
    List.max(lst = List(1,2,3,4)) shouldEqual 4}
  "El minimo de la lista" should "1" in {
    List.min(lst = List(1,2,3,4)) shouldEqual 1}
  "and de la lista" should "true" in {
    List.and(lst = List(true,true,true)) shouldEqual true}
  "or de la lista" should "false" in {
    List.and(lst = List(false,false,false)) shouldEqual false}
  "maxMin de la lista" should "(1,4)" in {
    List.minMax(lst = List(1,2,3,4)) shouldEqual (1,4)}
}
