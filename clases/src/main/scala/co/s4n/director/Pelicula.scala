package co.s4n.director

class Pelicula(val nombre:String,
               val presentacion:Int,
               val rangoIMDB:Double,
               val director: Director){
  def directorEdad = presentacion - director.nacimiento
  def esDirigidaPor(director: Director) = this.director == director
  def copy(nombre:String = this.nombre,
           presentacion:Int = this.presentacion,
           rangoIMDB:Double = this.rangoIMDB,
           director: Director = this.director):Pelicula = new Pelicula(nombre,presentacion,rangoIMDB,director)
}
object Pelicula extends App {
  def apply(nombre: String, presentacion: Int, rangoIMDB: Double, director: Director): Pelicula = new Pelicula(nombre, presentacion, rangoIMDB, director)
  def mejorCalificada(pelicula1:Pelicula,pelicula2:Pelicula):String = if(pelicula1.rangoIMDB > pelicula2.rangoIMDB) pelicula1.nombre else pelicula2.nombre
  def mayorDirectorEnElTiempo(pelicula1:Pelicula,pelicula2:Pelicula):String =
    if(pelicula1.directorEdad > pelicula2.directorEdad) pelicula1.nombre
    else pelicula2.nombre
  val pelicula1 = Pelicula("harry",2,5,new Director("Laura","Carmona",20))
  val pelicula2 = Pelicula("potter",2,7,new Director("juan","Carmona",30))
  println(mayorDirectorEnElTiempo(pelicula1,pelicula2))
}