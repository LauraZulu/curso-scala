package co.s4n.contador

class Contador(contador:Int) {
  def incr(int: Int = 1):Contador = new Contador(contador+int)
  def decr(int: Int = 1):Contador = new Contador(contador-int)
  def contador():Int = contador
  def ajuste(sumador: Sumador):Contador = new Contador(sumador.adicionar(contador))
}
class Sumador(monto:Int){
  def adicionar(valor:Int) = valor + monto
}
class Contador1(contador:Int) {
  def incr():Contador = new Contador(contador+1)
  def decr():Contador = new Contador(contador-1)
  def contador():Int = contador
}
object Contador extends App {
  val contador1 = new Contador(20)
  println(contador1.decr().incr().decr().incr().contador())
  println(contador1.ajuste(new Sumador(3)).contador())
}