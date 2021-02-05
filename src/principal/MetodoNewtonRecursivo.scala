package principal

object MetodoNewtonRecursivo extends App{
  def obtenerValoresAbsolutos(n: Double) = if (n > 0) n else -n

  def esNumeroAdecuado(numero_aproximado: Double, n: Double) = obtenerValoresAbsolutos(numero_aproximado *
  numero_aproximado - n) / n <  0.0000001

  def agregarCifras(numero_aproximado: Double, n: Double) = (numero_aproximado + n / numero_aproximado) / 2

  def metodoNewton(numero_aproximado: Double, n: Double): Double = {
    if (esNumeroAdecuado(numero_aproximado,n)) numero_aproximado
    else metodoNewton(agregarCifras(numero_aproximado,n), n)
  }

  def obtenerRaizCuadrada(n: Double): Double = metodoNewton(1, n)

  // PRUEBAS

  println(obtenerRaizCuadrada(150))



}
