package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class funciones_test extends AnyFlatSpec with Matchers{

  "El area de un circulo con radio 1" should "PI" in {
    FuncionesScala.areaDeUnCirculo(1) shouldEqual Math.PI}
  "El area de un triangulo rectangulo con lados 2,3" should "3" in {
    FuncionesScala.areaTrianguloRectangulo (2,3) shouldEqual 3}
  "El calculo del salario con devengado 20 y deducciones 10" should "10" in {
    FuncionesScala.calSalario (20,10) shouldEqual 10}
  "El calculo del salario con bono 1.10" should "12" in {
    FuncionesScala.calSalarioBono (20,10) shouldEqual 12}
  "El calculo del salario utilizando un bono de 5%" should "11" in {
    FuncionesScala.calSalario5 (20,10) shouldEqual 11}
  "El calculo del salario utilizando un bono de 20%" should "14" in {
    FuncionesScala.calSalario20 (20,10) shouldEqual 14}
  "El calculo del compSalario con calSalario como arg" should "800000" in {
    FuncionesScala.compSalario(FuncionesScala.calSalario, 1000000, 200000) shouldEqual 800000}
  "El calculo del compSalario con calSalarioBono como arg" should "900000" in {
    FuncionesScala.compSalario(FuncionesScala.calSalarioBono, 1000000, 200000) shouldEqual 900000}
  "El calculo del salario utilizando un bono como clausura" should "14" in {
    FuncionesScala.calSalarioBonoClausura (20,10) shouldEqual 14}
  "El calculo del compSalario utilizando calSalarioBonoClausura" should "14" in {
    FuncionesScala.compSalario(FuncionesScala.calSalarioBonoClausura,20,10) shouldEqual 14}
  "El calculo de la funcion factorial de 2" should "2" in {
    FuncionesScala.factorial(2) shouldEqual 2}
  "El calculo de la funcion f de 2" should "1" in {
  FuncionesScala.f(2) shouldEqual 1}
  "El calculo de la funcion factorialTail de 3" should "3" in {
    FuncionesScala.factorialTail(3,1) shouldEqual 6}
}
