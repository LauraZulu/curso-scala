package co.s4n.editorial

object editorial extends App {
  def presentar(b:Libro) = "Presentamos a " + b.titulo +
                                " de " + b.autor +
                            " un ejemplar de " + b.referencia
  val libro = new Libro("Cien años de soledad", "Gabriel Garcia Marques", "Realismo magico")
  var libro2 = new Libro(autor = "Gabriel Garcia", titulo = "Cien años de soledad")
  println(editorial.presentar(libro))
  println(editorial.presentar(libro2))
}
