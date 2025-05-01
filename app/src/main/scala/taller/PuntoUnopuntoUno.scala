package taller

object PuntoUnopuntoUno {

  type Vagon = Any
  type Tren = List[Vagon]
  type Estado = (Tren, Tren, Tren)

  trait Movimiento
  case class Uno(n: Int) extends Movimiento
  case class Dos(n: Int) extends Movimiento

  def aplicarMovimiento(estado: Estado, movimiento: Movimiento): Estado = {
    val (principal, uno, dos) = estado

    movimiento match {
      case Uno(n) if n > 0 =>
        val (mover, mantener) = principal.splitAt(n)
        (mantener, mover ++ uno, dos)

      case Uno(n) if n < 0 =>
        val (mover, mantener) = uno.splitAt(-n)
        (mover ++ principal, mantener, dos)

      case Dos(n) if n > 0 =>
        val (mover, mantener) = principal.splitAt(n)
        (mantener, uno, mover ++ dos)

      case Dos(n) if n < 0 =>
        val (mover, mantener) = dos.splitAt(-n)
        (mover ++ principal, uno, mantener)

      case _ => estado // Movimiento Uno(0) o Dos(0) no tiene efecto
    }
  }

  def generarMovimientos(trenInicial: Tren, trenDeseado: Tren): List[Movimiento] = {
    if (trenInicial == trenDeseado) {
      List()
    } else {
      // Estrategia simple: mover todos los vagones a la vía 1 y luego traerlos en orden inverso
      val moverATodosAUno = Uno(trenInicial.length)

      // Para cada vagón en el tren deseado (en orden inverso), traerlo de la vía 1
      val traerVagones = trenDeseado.reverse.map { vagon =>
        // Encontrar la posición del vagón en la vía 1
        val posicion = trenInicial.indexOf(vagon) + 1
        Uno(-posicion)
      }

      moverATodosAUno :: traerVagones
    }
  }

  def maniobrarTren(trenInicial: Tren, trenDeseado: Tren): (Estado, List[Movimiento]) = {
    require(trenInicial.toSet == trenDeseado.toSet, "Los trenes deben tener los mismos vagones")

    val estadoInicial: Estado = (trenInicial, List(), List())
    val movimientos = generarMovimientos(trenInicial, trenDeseado)

    val estadoFinal = movimientos.foldLeft(estadoInicial) { (estado, movimiento) =>
      aplicarMovimiento(estado, movimiento)
    }

    (estadoFinal, movimientos)
  }
}