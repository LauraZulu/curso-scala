package co.s4n.traits

trait Felino {
  def color:String
  def sonido:String
}
case class Leon() extends Felino {
  override def color:String = "Amarillo"
  override def sonido:String = "Grr"
  def tamañoDeMelena:String = "Grande"
}
case class Tigre() extends Felino {
  override def color: String = "Gris"
  override def sonido: String = "Rrrrr"
}
case class Jaguar() extends Felino {
  override def color: String = "Amarillo"
  override def sonido: String = "Miaaa"
}
case class Gato() extends Felino {
  override def color: String = "Negro"
  override def sonido: String = "Miaaauuu"
  def comidaFavorita:String = "atun"
}