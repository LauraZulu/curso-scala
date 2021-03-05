package co.s4n.traits

class Color(val red:Int,val green:Int, val blue:Int) {

}
object Color extends App {
  def apply(red: Int, green:Int, blue:Int) = new Color (red, green, blue)
}
object Red extends Color(255,0,0)
object Yellow extends Color(255,255,0)
object Pink extends Color(255,0,255)

