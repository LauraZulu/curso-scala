package co.s4n.listas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class listasSpec extends AnyFlatSpec with Matchers{
  "Las posibles combinaciones de una lista" should "List(List(), List(1), List(2), List(1, 2))" in {
    lista.subs(List(1,2)) shouldEqual List(List(), List(1), List(2), List(1, 2))}
  "Las posibles permutaciones de una lista" should "List(List(1, 2), List(2, 1))" in {
    lista.permutaciones(List(1,2)) shouldEqual List(List(1, 2), List(2, 1))}
  "Las posibles permutaciones de una lista usando barajar" should "List(List(1, 2), List(2, 1))" in {
    lista.permutaciones2(List(1,2)) shouldEqual List(List(1, 2), List(2, 1))}
  "El ultimo elemento de una lista" should "2" in {
    lista.lastF(List(1,2)) shouldEqual 2}
  "El n elemento de una lista" should "1" in {
    lista.returnElemen(1,List(1,2)) shouldEqual 1}
  "Longitud de la lista" should "5" in {
    lista.lengthL(List(1,2,4,5,6)) shouldEqual 5
    lista.lengthR(List(1,2,4,5,6)) shouldEqual 5}
  "El reverso de una lista" should "List(5,4,3,2,1)" in {
   lista.reversel(List(1,2,3,4,5)) shouldEqual List(5,4,3,2,1)}
  "La lista es palindrome" should "false" in {
   lista.isPalindrome(List(1,2,3,4,5)) shouldEqual false}
   "La función pack" should "List(List(1,1),List(2,2))" in {
    lista.pack(List(1,1,2,2)) shouldEqual List(List(1,1),List(2,2))}
    "La función encode" should "List((3,1), (2,2))" in {
    lista.encode(List(1,1,1,2,2)) shouldEqual List((3,1), (2,2))}
     "La función dupli" should "List(1,1,2,2,3,3)" in {
     lista.dupli(List(1,2,3)) shouldEqual List(1,1,2,2,3,3)}
}
