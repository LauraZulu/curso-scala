package co.s4n.director

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class director extends AnyFlatSpec with Matchers{
  "Direcrtor mayot" should "Juan" in {
    Director.esMayor(Director("Juan","Camilo",30), Director("Laura","Vanessa",20)) shouldEqual "Juan"  }
    val pelicula1 = Pelicula("harry",2,5,new Director("Laura","Carmona",20))
    val pelicula2 = Pelicula("potter",2,7,new Director("juan","Carmona",30))
  "Pelicula mejor calificada" should "potter" in {
    Pelicula.mejorCalificada(pelicula1,pelicula2) shouldEqual "potter"  }
  "Pelicula con mayor director" should "Un circulo de radio radio 2" in {
    Pelicula.mayorDirectorEnElTiempo(pelicula1,pelicula2)shouldEqual "harry"}
}
