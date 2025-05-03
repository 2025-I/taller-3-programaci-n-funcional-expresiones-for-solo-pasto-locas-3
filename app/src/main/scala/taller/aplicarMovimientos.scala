package taller

import scala.annotation.tailrec
import Modelos._

class aplicarMovimientos {
  private val mover = new aplicarMovimiento

  def aplicarMovimientos(e: Estado, movimientos: List[Movimiento]): List[Estado] = {
    @tailrec
    def aplicarAux(pendientes: List[Movimiento], acumulados: List[Estado]): List[Estado] = {
      pendientes match {
        case Nil => acumulados.reverse
        case mov :: resto =>
          val siguiente = mover.aplicarMovimiento(acumulados.head, mov)
          aplicarAux(resto, siguiente :: acumulados)
      }
    }

    aplicarAux(movimientos, List(e))
  }
}
