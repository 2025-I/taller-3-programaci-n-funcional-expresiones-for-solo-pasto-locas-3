package taller

import Modelos._

class aplicarMovimiento {
  def aplicarMovimiento(e: Estado, m: Movimiento): Estado = {
    val (main, uno, dos) = e

    m match {
      case Uno(n) if n > 0 =>
        val (newMain, moved) = main.splitAt(main.length - n)
        (newMain, moved ++ uno, dos)

      case Uno(n) if n < 0 =>
        val takeN = math.min(-n, uno.length)
        val (moved, newUno) = uno.splitAt(takeN)
        (main ++ moved, newUno, dos)

      case Dos(n) if n > 0 =>
        val (newMain, moved) = main.splitAt(main.length - n)
        (newMain, uno, moved ++ dos)

      case Dos(n) if n < 0 =>
        val takeN = math.min(-n, dos.length)
        val (moved, newDos) = dos.splitAt(takeN)
        (main ++ moved, uno, newDos)

      case _ => e  // Si el movimiento no es válido, no se hace nada
    }
  }
}
