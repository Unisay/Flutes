package com.github.unisay.flutes

import java.lang.Math.PI

import org.scalajs.dom
import org.scalajs.dom._
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object FluteApp extends JSApp {

  type C2D = CanvasRenderingContext2D
  implicit def intsToPoint(p: (Int, Int)): Point = Point(p._1, p._2)
  implicit def doublesToPoint(p: (Double, Double)): Point = Point(p._1, p._2)

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

  @JSExport
  override def main() = {
    val canvas = dom.document.getElementById("fluteCanvas").asInstanceOf[html.Canvas]
    implicit val context2D = canvas.getContext("2d").asInstanceOf[C2D]
    drawFlutes((1, 20), parseTones)
  }

  private def parseTones: Seq[Seq[Tone.Value]] = {
    println(window.location.search.stripPrefix("?tones="))
    window.location.search.stripPrefix("?tones=").split("%0D%0A").toSeq.map {
      _.filter("abcdefgABCDEFG".contains(_)).map(_.toString).map(Tone.withName)
    }
  }

  private def drawFlutes(start: Point, toneRows: Seq[Seq[Tone.Value]])(implicit context: C2D) =
    toneRows.foldLeft(start) { (point, tones) => drawRow(point, tones); point down 200 }

  private def drawRow(start: Point, tones: Seq[Tone.Value])(implicit context: C2D) =
    tones.foldLeft(start) { (point, tone) => drawFlute(point, tone); point right 60 }

  private def drawFlute(upperLeft: Point, tone: Tone.Value, width: Int = 26, height: Int = 160)(implicit context: C2D) = {
    val halfWidth = width / 2

    def drawHole(offset: Int, closed: Int)(implicit context: C2D) =
      circle(upperLeft right halfWidth down offset, halfWidth / 3, if (closed == 1) Some("#888") else None)

    def drawTone(tone: Tone.Value) = {
      val offsets = List(0, 30, 45, 60, 90, 105, 120, 150)
      for (h <- offsets.zip(states(tone))) drawHole(h._1, h._2)
    }

//    val rectUpperLeft = upperLeft down 10
//    context.save()
//    context.beginPath()
//    context.fillStyle = "#DDDDDD"
//    context.fillRect(rectUpperLeft.x, rectUpperLeft.y, width, height)
//    context.shadowBlur = 10
//    context.shadowColor = "#555555"
//    context.stroke()
//    context.restore()

    val p = upperLeft

    context.beginPath()
    //context.moveTo(p.x + 5, p.y)
    //context.lineTo(p.x + 25, p.y)
    context.arcTo(p.x + 25, p.y, p.x + 30, p.y, 50)
    context.stroke()
    drawTone(tone)

    //    val label = upperLeft down height down 30 right halfWidth / 2
//    context.font = "bold 14pt Arial"
//    context.fillText(tone.toString, label.x, label.y)
  }

  private def circle(center: Point, radius: Double, fillColor: Option[String] = None)(implicit context: C2D) = {
    context.beginPath()
    context.arc(center.x, center.y, radius, 0, 2 * PI)
    fillColor.foreach { color =>
      context.fillStyle = color
      context.fill()
    }
    context.stroke()
  }

}

case class Point(x: Double, y: Double) {
  def up(d: Double) = Point(x, y - d)
  def down(d: Double) = Point(x, y + d)
  def left(d: Double) = Point(x - d, y)
  def right(d: Double) = Point(x + d, y)
}