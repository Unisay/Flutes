package com.github.unisay.flutes

import java.lang.Math.PI

import org.scalajs.dom
import org.scalajs.dom._
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object FluteApp extends JSApp {

  type C2D = CanvasRenderingContext2D
  implicit def intsToVector2d(vector: (Int, Int)): Vector2d = Vector2d(vector._1, vector._2)
  implicit def doublesToVector2d(vector: (Double, Double)): Vector2d = Vector2d(vector._1, vector._2)
  implicit val scale = Scale(1)

  object Tone extends Enumeration {
    val
    c, d, e, f, g, a, b,
    C, D, E, F, G, A, B = Value
  }

  val states = Map(

    // lower octave
    (Tone.c, List(1, 1, 1, 1, 1, 1, 1, 1)),
    (Tone.d, List(1, 1, 1, 1, 1, 1, 1, 0)),
    (Tone.e, List(1, 1, 1, 1, 1, 1, 0, 0)),
    (Tone.f, List(1, 1, 1, 1, 1, 0, 0, 0)),
    (Tone.g, List(1, 1, 1, 1, 0, 0, 0, 0)),
    (Tone.a, List(1, 1, 1, 0, 0, 0, 0, 0)),
    (Tone.b, List(1, 1, 0, 0, 0, 0, 0, 0)),

    // higher octave
    (Tone.C, List(1, 0, 1, 0, 0, 0, 0, 0)),
    (Tone.D, List(0, 0, 1, 0, 0, 0, 0, 0)),
    (Tone.E, List(0, 1, 1, 1, 1, 1, 0, 0)), // todo 1 or 2
    (Tone.F, List(0, 1, 1, 1, 1, 0, 0, 0)), // todo 1 or 2
    (Tone.G, List(0, 1, 1, 1, 0, 0, 0, 0)), // todo 1 or 2
    (Tone.A, List(0, 1, 1, 0, 0, 0, 0, 0)), // todo ?
    (Tone.B, List(0, 1, 0, 0, 0, 0, 0, 0))) // todo ?

  val fluteWidth = 30
  val fluteHeight = 160

  @JSExport
  def resizeCanvas(canvas: html.Canvas)(implicit context: C2D): Unit = {
    canvas.width = window.innerWidth
    canvas.height = window.innerHeight
    drawFlutes((15, 25), parseTones)
  }

  @JSExport
  override def main() = {
    val canvas = dom.document.getElementById("fluteCanvas").asInstanceOf[html.Canvas]
    implicit val context2D = canvas.getContext("2d").asInstanceOf[C2D]
    val render = (_: Event) ⇒ resizeCanvas(canvas)
    window.addEventListener("resize", render, useCapture = false)
    render(null)
  }

  private def parseTones: Seq[Seq[Tone.Value]] = {
    window.location.search.stripPrefix("?tones=").split("%0D%0A").toSeq.map {
      _.filter("abcdefgABCDEFG".contains(_)).map(_.toString).map(Tone.withName)
    }
  }

  private def drawFlutes(start: Vector2d, toneRows: Seq[Seq[Tone.Value]])(implicit context: C2D) =
    toneRows.foldLeft(start) { (point, tones) => drawRow(point, tones); point down fluteHeight down 40 }

  private def drawRow(start: Vector2d, tones: Seq[Tone.Value])(implicit context: C2D) =
    tones.foldLeft(start) { (point, tone) => drawFlute(point, tone); point right fluteWidth * 2 }

  private def drawFlute(upperLeft: Vector2d, tone: Tone.Value)(implicit context: C2D) = {
    context.strokeStyle = "black"

    def drawHole(v: Vector2d, closed: Int, radius: Double = 4.5)(implicit context: C2D) =
      circle(v, radius, if (closed == 1) Some("#222") else None)

    def drawHoles(v: Vector2d, tone: Tone.Value) = {
      val ss = states(tone).iterator
      drawHole(v up 15, ss.next(), 3)
      drawHole(v down 15, ss.next())
      drawHole(v down 35, ss.next())
      drawHole(v down 55, ss.next())
      drawHole(v down 80, ss.next())
      drawHole(v down 100, ss.next())
      drawHole(v down 120, ss.next())
      drawHole(v down 140, ss.next())
    }

    val downLeft = upperLeft down fluteHeight
    val upperRight = upperLeft right fluteWidth
    val downRight = upperRight down fluteHeight
    val vShift = 10

    strokePath { _ ⇒
      line(upperLeft, downLeft)
      bezierTo(downLeft up vShift, downRight up vShift, downRight)
      lineTo(upperRight)
      bezierTo(upperRight up vShift, upperLeft up vShift, upperLeft)

      fillGradient(upperLeft, upperRight, (0.05, "#DDDDDD"), (0.50, "white"), (0.95, "#DDDDDD"))
    }

    strokePath { _ ⇒
      moveTo(downLeft)
      bezierTo(downLeft up vShift, downRight up vShift, downRight)
      bezierTo(downRight down vShift, downLeft down vShift, downLeft)

      fillGradient(downLeft up vShift, downLeft down vShift, (0.35, "white"), (0.95, "darkgray"))
    }

    drawHoles(upperLeft right fluteWidth / 2, tone)
  }

  private def fillGradient(g1: Vector2d, g2: Vector2d, colorStops: (Double, String)*)(implicit context: C2D) = {
    val gradient = context.createLinearGradient(g1.x, g1.y, g2.x, g2.y)
    colorStops.foreach(stop ⇒ gradient.addColorStop(stop._1, stop._2))
    fill(gradient)
  }

  private def circle(center: Vector2d, radius: Double, fillColor: Option[String] = None)(implicit context: C2D) = {
    strokePath { implicit context ⇒
      context.arc(center.x, center.y, radius, 0, 2 * PI)
      fillColor foreach fill
    }
  }

  private def fill(style: String)(implicit context: C2D) = {
    context.fillStyle = style
    context.fill()
  }

  private def fill(gradient: CanvasGradient)(implicit context: C2D) = {
    context.fillStyle = gradient
    context.fill()
  }

  private def moveTo(v: Vector2d)(implicit context: C2D, scale: Scale) =
    context.moveTo(v.x * scale.x, v.y * scale.y)

  private def lineTo(v: Vector2d)(implicit context: C2D, scale: Scale) =
    context.lineTo(v.x * scale.x, v.y * scale.y)

  private def line(from: Vector2d, to: Vector2d)(implicit context: C2D, scale: Scale) = {
    moveTo(from)
    lineTo(to)
  }

  private def bezierTo(c1: Vector2d, c2: Vector2d, to: Vector2d)(implicit context: C2D) =
    context.bezierCurveTo(
      c1.x * scale.x, c1.y * scale.y,
      c2.x * scale.x, c2.y * scale.y,
      to.x * scale.x, to.y * scale.y)

  private def strokePath(f: C2D => Unit)(implicit context: C2D) = {
    context.beginPath()
    f(context)
    context.closePath()
    context.stroke()
  }

}

object Scale {
  def apply(factor: Double) = new Scale(factor, factor)
}

case class Scale(x: Double, y: Double)

case class Vector2d(x: Double, y: Double) {
  def up(d: Double) = Vector2d(x, y - d)
  def down(d: Double) = Vector2d(x, y + d)
  def left(d: Double) = Vector2d(x - d, y)
  def right(d: Double) = Vector2d(x + d, y)
}