package taller

import scala.annotation.tailrec
import Modelos._

class definirManiobra {
  type Maniobra = List[Movimiento]

  def definirManiobra(t1: Tren, t2: Tren): Maniobra = {
    @tailrec
    def construir(actual: Tren, objetivo: Tren, uno: Tren, dos: Tren, acc: Maniobra): Maniobra = {
      (actual, objetivo) match {
        case (Nil, Nil) => acc.reverse
        case (aHead :: aTail, oHead :: oTail) if aHead == oHead =>
          construir(aTail, oTail, uno, dos, acc)
        case _ =>
          (for {
            mov <- actual.headOption.map(_ => Uno(1)) orElse {
              if (uno.nonEmpty && uno.head == objetivo.head) Some(Uno(-1))
              else if (dos.nonEmpty && dos.head == objetivo.head) Some(Dos(-1))
              else None
            }
            nextState = mov match {
              case Uno(1) =>
                val mover = actual.takeRight(1)
                val nuevoActual = actual.dropRight(1)
                (nuevoActual, objetivo, mover ++ uno, dos, mov :: acc)
              case Uno(-1) =>
                val mover = uno.head
                (mover :: actual, objetivo.tail, uno.tail, dos, mov :: acc)
              case Dos(-1) =>
                val mover = dos.head
                (mover :: actual, objetivo.tail, uno, dos.tail, mov :: acc)
              case Dos(n) =>
                val nuevoDos = uno ++ dos
                (actual, objetivo, Nil, nuevoDos, mov :: acc)
            }
          } yield nextState) match {
            case Some((a, o, u, d, aMov)) => construir(a, o, u, d, aMov)
            case None => Nil
          }
      }
    }

    if (t1 == t2) Nil
    else if (t1.reverse == t2) {
      List(Uno(t1.length), Dos(t1.length), Uno(-t1.length), Dos(-t1.length))
    } else {
      construir(t1, t2, Nil, Nil, Nil)
    }
  }
}
