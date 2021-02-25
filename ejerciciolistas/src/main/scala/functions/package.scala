package functions

object FuncionesScala {
  // Funcion literal área del triangulo
  val areaTrianguloRectangulo = (a:Int, b:Int) => {
      (a*b)/ 2
  }
  // Funcion literal de la clase Function1
  val areaDeUnCirculo = new Function1[Double,Double]{
    def apply(r: Double) =
    {
      Math.PI*r*r
    }}
  // Funcion literal a para calcular salario
  val calSalario = (devengado:Double, deducciones:Double) => {
      devengado - deducciones
  }
  // Funcion literal para calcular salario utilizando un valor de bono
  val calSalarioBono = (devengado:Double, deducciones:Double) =>
  {
    (devengado*1.10) - deducciones
  }
  // Funcion que tiene como argumento una función

  def compSalario(f:(Double,Double) => Double, devengado:Double, deducciones:Double) = f(devengado,deducciones)

  // Prueba de la funcion compSalario con las funciones calSalario y calSalarioBono como argumentos

  compSalario(calSalario, 1000000, 200000)
  compSalario(calSalarioBono, 1000000, 200000)

  // Funcion que genera una funcion para calcular salario utilizando bono
  def genCalSalarioBono(bono:Double):(Double, Double) => Double =
  {
    (devengado:Double, deducciones:Double) => {(devengado*bono) - deducciones}
  }

  // Funciones literales utilizando la funcion genCalSalarioBono
  val calSalario5 = genCalSalarioBono(1.05)
  val calSalario20 = genCalSalarioBono(1.2)

  // Funcion utilizando bono como una clausura
  val bono = 1.2
  val calSalarioBonoClausura = (devengado:Double,deducciones:Double) =>
  {
    (devengado*bono) - deducciones
  }
  // Funcion compSalario con calSalarioBonoClausura como arg
  compSalario(calSalarioBonoClausura,200000,10000)

  // Funciones parcialmente aplicadas
  def calSalario15():(Double,Double)=>Double = {genCalSalarioBono(0.15)}
  def calSalario100():(Double,Double)=>Double = {genCalSalarioBono(2.0)}

  // Funcion recursiva factorial
  def factorial(n:Int):Int =
  {
    if (n == 0) 1
    else if (n == 1) 1
    else n*factorial(n-1)
  }

  // Funcion recursiva
  def f(n:Int):Int =
  {
    if (n == 0) 0
    else if (n == 1) 1
    else f(n-1)+f(n-2)
  }

  // Funcion con recursividad de cola
  def factorialTail(n:Int):Int ={
    def f(n:Int, acum:Int): Int ={
      if(n == 0) acum
      else if(n == 1) acum
      else f(n-1,acum*n)
    }
    f(n,1)
  }
}
