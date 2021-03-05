package co.s4n.classes

class Gato(val nombre:String, val color:String, val comida:String) {
}
object Gato extends App {
  val gato1 = new Gato("IO", "Fawn","Churrus")
  val gato2 = new Gato("Make", "Red","Leche")
  val gato3 = new Gato("Docker", "Blue", "Cuido")
}