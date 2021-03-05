package co.s4n.persona

class Persona(val nombre:String, val apellido:String){
  def nombreString:String = s"$nombre $apellido"
}
object Persona extends App {
 def apply(nombre:String):Array[String] = nombre.split(" ")
}
