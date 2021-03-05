package co.s4n.director

class Director(
              val nombre:String,
              val apellido:String,
              val nacimiento:Int
              ) {
def nombreCompleto:String = s"$nombre $apellido"
def copy(nombre:String = this.nombre,
        apellido:String = this.apellido,
         nacimiento:Int = this.nacimiento):Director = new Director(nombre,apellido,nacimiento)
}
object Director extends App{
  def apply(nombre:String, apellido:String, nacimiento:Int):Director = new Director(nombre,apellido,nacimiento)
  def esMayor(director1:Director, director2:Director):String = if (director1.nacimiento > director2.nacimiento) director1.nombre else director2.nombre
  println(Director.esMayor(director2 = Director("Juan","Camilo",12201995), director1 = Director("Laura","Vanessa",12011996)))
}
