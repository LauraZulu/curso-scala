package co.s4n.traits

sealed trait Forma {
 def tamaño:Double
  def perimetro:Double
  def area:Double
  def descripcion:String
}
sealed trait Rectangular extends Forma{
  override def tamaño: Double = 4.0
}
case class Circulo(radio:Double) extends Forma{
  override def tamaño:Double = 0.0
  override def perimetro: Double = 2 * math.Pi * radio
  override def area:Double = math.Pi * math.pow(radio,2)
  override def descripcion: String = s"Un circulo de radio $radio"
}
case class Rectangulo(base:Double, altura:Double) extends Rectangular {
  override def perimetro: Double = 2*(base+altura)
  override def area:Double = base * altura
  override def descripcion: String = s"Un Rectangulo de ancho $altura y de largo $base"
}
case class Cuadrado(lado:Double) extends Rectangular {
  override def perimetro: Double = lado+lado+lado+lado
  override def area:Double = lado * lado
  override def descripcion: String = s"Un cuadrado de lado $lado"
}

object Draw extends App {
  def apply(forma: Forma): String = forma.descripcion
  def apply2(forma: Forma): String = forma match {
    case Circulo(radio) => s"Un circulo de radio $radio"
    case Rectangulo(base,altura) => s"Un Rectangulo de ancho $altura y de largo $base"
    case Cuadrado(lado) => s"Un cuadrado de lado $lado"
  }
}