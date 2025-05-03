package taller

object Modelos {

  /** Tipo que representa un vagón de tren. */
  type Vagon = Any

  /** Un tren es una lista de vagones. */
  type Tren = List[Vagon]

  /** Estado del sistema: (en la vía principal, en la vía uno, en la vía dos). */
  type Estado = (Tren, Tren, Tren)

  /** Movimiento posible del sistema. */
  sealed trait Movimiento

  /** Movimiento entre vía principal y vía uno.
   * n > 0: de principal a uno.
   * n < 0: de uno a principal.
   */
  case class Uno(n: Int) extends Movimiento

  /** Movimiento entre vía principal y vía dos.
   * n > 0: de principal a dos.
   * n < 0: de dos a principal.
   */
  case class Dos(n: Int) extends Movimiento
}
