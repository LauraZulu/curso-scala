package co.s4n.editorial

class Libro(val titulo:String, val autor:String, val referencia:String) {
    def this(titulo:String,autor:String) = this(titulo,autor,"Ficcion")
    def nombre = s"$titulo $autor $referencia"
}
