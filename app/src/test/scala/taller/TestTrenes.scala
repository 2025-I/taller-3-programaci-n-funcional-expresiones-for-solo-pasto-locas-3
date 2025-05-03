package taller

import org.junit.Test
import org.junit.Assert._
import taller.Modelos._

class TestTrenes {

  val objAplicarMov = new aplicarMovimiento()
  val objAplicarMovs = new aplicarMovimientos()
  val objDefinirManiobra = new definirManiobra()

  @Test
  def aplicarMovimientoUno2(): Unit = {
    val estadoInicial = (List(1, 2, 3, 4), Nil, Nil)
    val estadoFinal = objAplicarMov.aplicarMovimiento(estadoInicial, Uno(2))
    assertEquals((List(1, 2), List(3, 4), Nil), estadoFinal)
  }

  @Test
  def aplicarMovimientoUnoNeg1(): Unit = {
    val estadoInicial = (List(1, 2), List(3), Nil)
    val estadoFinal = objAplicarMov.aplicarMovimiento(estadoInicial, Uno(-1))
    assertEquals((List(1, 2, 3), Nil, Nil), estadoFinal)
  }

  @Test
  def aplicarMovimientoDos2(): Unit = {
    val estadoInicial = (List(1, 2, 3), Nil, Nil)
    val estadoFinal = objAplicarMov.aplicarMovimiento(estadoInicial, Dos(2))
    assertEquals((List(1), Nil, List(2, 3)), estadoFinal)
  }

  @Test
  def aplicarMovimientoDosNeg1(): Unit = {
    val estadoInicial = (List(1), Nil, List(2, 3))
    val estadoFinal = objAplicarMov.aplicarMovimiento(estadoInicial, Dos(-1))

    // Verifica que el valor final sea el esperado
    assertEquals("El estado final no es el esperado", (List(1, 2), Nil, List(3)), estadoFinal)
  }

  @Test
  def aplicarMovimientoUno0(): Unit = {
    val estadoInicial = (List(1, 2, 3), List(4), List(5))
    val estadoFinal = objAplicarMov.aplicarMovimiento(estadoInicial, Uno(0))
    assertEquals(estadoInicial, estadoFinal)
  }

  @Test
  def aplicarMultiplesMovimientos(): Unit = {
    val estadoInicial = (List(1, 2, 3, 4), Nil, Nil)
    val movimientos = List(Uno(2), Dos(1), Uno(-1), Dos(-1))
    val estados = objAplicarMovs.aplicarMovimientos(estadoInicial, movimientos)
    assertEquals((List(1, 3, 2), List(4), Nil), estados.last)
  }

  @Test
  def aplicarMovimientosVacios(): Unit = {
    val estadoInicial = (List(1, 2, 3), Nil, Nil)
    val estados = objAplicarMovs.aplicarMovimientos(estadoInicial, Nil)
    assertEquals(estadoInicial, estados.last)
  }

  @Test
  def invertirTren(): Unit = {
    val estadoInicial = (List(1, 2, 3), Nil, Nil)
    val movimientos = List(Uno(3), Dos(3), Uno(-3), Dos(-3))
    val estados = objAplicarMovs.aplicarMovimientos(estadoInicial, movimientos)
    assertEquals((List(1, 2, 3), Nil, Nil), estados.last)
  }

  @Test
  def movimientosConCero(): Unit = {
    val estadoInicial = (List(1, 2, 3), Nil, Nil)
    val movimientos = List(Uno(0), Dos(0))
    val estados = objAplicarMovs.aplicarMovimientos(estadoInicial, movimientos)
    assertEquals(estadoInicial, estados.last)
  }

  @Test
  def trenVacio(): Unit = {
    val estadoInicial = (Nil, Nil, Nil)
    val movimientos = List(Uno(1), Dos(-1))
    val estados = objAplicarMovs.aplicarMovimientos(estadoInicial, movimientos)
    assertEquals(estadoInicial, estados.last)
  }

  @Test
  def maniobraInversion(): Unit = {
    val trenInicial = List(1, 2, 3, 4)
    val trenObjetivo = trenInicial.reverse
    val maniobra = objDefinirManiobra.definirManiobra(trenInicial, trenObjetivo)
    assertEquals(List(Uno(4), Dos(4), Uno(-4), Dos(-4)), maniobra)
  }

  @Test
  def maniobraElementoUnico(): Unit = {
    val maniobra = objDefinirManiobra.definirManiobra(List(1), List(1))
    assertTrue(maniobra.isEmpty)
  }

  @Test
  def maniobraTrenVacio(): Unit = {
    val maniobra = objDefinirManiobra.definirManiobra(Nil, Nil)
    assertTrue(maniobra.isEmpty)
  }



  @Test
  def maniobraTrenesIguales(): Unit = {
    val tren = List(1, 2, 3)
    val maniobra = objDefinirManiobra.definirManiobra(tren, tren)
    assertTrue(maniobra.isEmpty)
  }

  @Test
  def pruebaJuguete(): Unit = {
    val estadoInicial = (List.range(1, 11), Nil, Nil)
    val movimientos = List.fill(10)(Uno(1))
    val estados = objAplicarMovs.aplicarMovimientos(estadoInicial, movimientos)
    assertTrue(estados.last._1.isEmpty)
  }

  @Test
  def pruebaPequena(): Unit = {
    val estadoInicial = (List.range(1, 101), Nil, Nil)
    val movimientos = List.fill(100)(Dos(1))
    val estados = objAplicarMovs.aplicarMovimientos(estadoInicial, movimientos)
    assertEquals(100, estados.last._3.size)
  }

  @Test
  def pruebaMediana(): Unit = {
    val estadoInicial = (List.range(1, 501), Nil, Nil)
    val movimientos = List.fill(500)(Uno(1))  // Cambiado de -1 a 1
    val estados = objAplicarMovs.aplicarMovimientos(estadoInicial, movimientos)
    assertEquals(500, estados.last._2.size)  // Verifica que hay 500 elementos en la vía uno
  }

  @Test
  def pruebaGrande(): Unit = {
    val estadoInicial = (List.range(1, 1001), Nil, Nil)
    val movimientos = List.fill(1000)(Dos(-1))
    val estados = objAplicarMovs.aplicarMovimientos(estadoInicial, movimientos)
    assertEquals(1000, estados.last._1.size)
  }
}
