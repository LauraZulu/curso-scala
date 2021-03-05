package co.s4n.inmutable.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class List_test extends AnyFlatSpec with Matchers{
  "head of the list" should "1" in {
    List.head(lst = List(1,2,3,4)) shouldEqual 1}
  "max of the list" should "4" in {
    List.max(lst = List(1,2,3,4)) shouldEqual 4}
  "min of the list" should "1" in {
    List.min(lst = List(1,2,3,4)) shouldEqual 1}
  "and of the list" should "true" in {
    List.and(lst = List(true,true,true)) shouldEqual true}
  "or of the list" should "false" in {
    List.or(lst = List(false,false,true)) shouldEqual true}
  "maxMin of the list" should "(1,4)" in {
    List.minMax(lst = List(1,2,3,4)) shouldEqual (1,4)}
  "add an element to the list" should "(2,1,2,3,4)" in {
    List.const(2,List(1,2,3,4)) shouldEqual (List(2,1,2,3,4))}
  "add an element to the end" should "(1,2,3,4,2)" in {
    List.addEndOther(List(1,2,3,4),2) shouldEqual (List(1,2,3,4,2))}
  "append two lists" should "(1,2,3,4,2,3,4)" in {
    List.append((List(1,2,3,4)),(List(2,3,4))) shouldEqual (List(1,2,3,4,2,3,4))}
  "init from a list" should "(1,2)" in {
    List.init(List(1,2,3,4)) shouldEqual (List(1,2,3))}
  "drop from a list" should "(3,4)" in {
    List.drop(2,List(1,2,3,4)) shouldEqual (List(3,4))}
  "split from a list" should "(List(1,2),List(3,4))" in {
    List.split(2,List(1,2,3,4)) shouldEqual (List(1,2),List(3,4))}
}
