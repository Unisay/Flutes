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
    val C, D, E, F, G, A, B = Value
  }

  val states = Map(
    (Tone.C, List(1, 0, 0, 0, 0, 0, 0)),
    (Tone.D, List(1, 1, 0, 0, 0, 0, 0)),
    (Tone.E, List(1, 1, 1, 0, 0, 0, 0)),
    (Tone.F, List(1, 1, 1, 1, 0, 0, 0)),
    (Tone.G, List(1, 1, 1, 1, 1, 0, 0)),
    (Tone.A, List(1, 1, 1, 1, 1, 1, 0)),
    (Tone.B, List(1, 1, 1, 1, 1, 1, 1))
  )

  @JSExport
  override def main() = {
    val tones = window.location.search.substring(1, 30).map(c => Tone.withName(c.toString))
    val canvas = dom.document.getElementById("fluteCanvas").asInstanceOf[html.Canvas]
    implicit val context2D = canvas.getContext("2d").asInstanceOf[C2D]
    drawFlutes((20, 20), tones.toSeq: _*)
  }

  def drawFlutes(upperLeft: Point, tones: Tone.Value*)(implicit context: C2D) =
    tones.foldLeft(upperLeft) { (point, tone) =>
      drawFlute(point, tone)
      point right 60
    }

  def drawFlute(upperLeft: Point, tone: Tone.Value, width: Int = 30, height: Int = 200)(implicit context: C2D) = {
    val halfWidth = width / 2
    val radius = (halfWidth, 7)

    def drawHole(offset: Int, closed: Int)(implicit context: C2D) =
      circle(upperLeft right halfWidth down offset, halfWidth / 3, if (closed == 1) Some("#888") else None)

    def drawTone(tone: Tone.Value) = {
      val offsets = List(20, 40, 60, 90, 110, 130, 160)
      for (h <- offsets.zip(states(tone))) drawHole(h._1, h._2)
    }

    line(start = upperLeft, end = upperLeft down height)
    line(start = upperLeft right width, end = upperLeft down height right width)
    ellipse(center = upperLeft right halfWidth, radius = radius, startAngle = PI, endAngle = 0)
    ellipse(center = upperLeft down height right halfWidth, radius = radius)
    drawTone(tone)
    val label = upperLeft down height down 30 right halfWidth / 2
    context.font = "bold 14pt Arial"
    context.fillText(tone.toString, label.x, label.y)
  }

  def line(start: Point, end: Point)(implicit context: C2D) = {
    context.beginPath()
    context.moveTo(start.x, start.y)
    context.lineTo(end.x, end.y)
    context.stroke()
  }

  def circle(center: Point, radius: Double, fillColor: Option[String] = None)(implicit context: C2D) = {
    context.beginPath()
    context.arc(center.x, center.y, radius, 0, 2 * PI)
    fillColor.foreach { color =>
      context.fillStyle = color
      context.fill()
    }
    context.stroke()
  }

  def arc(center: Point, radius: Double, startAngle: Double = 0, endAngle: Double = 2 * PI)(implicit context: C2D) = {
    context.beginPath()
    context.arc(center.x, center.y, radius, startAngle, endAngle)
    context.stroke()
  }

  def ellipse(center: Point, radius: Point, startAngle: Double = 0, endAngle: Double = 2 * PI)(implicit context: C2D) {
    context.save()
    context.beginPath()
    context.translate(center.x - radius.x, center.y - radius.y)
    context.scale(radius.x, radius.y)
    context.arc(x = 1, y = 1, radius = 1, startAngle = startAngle, endAngle = endAngle)
    context.restore()
    context.save()
    context.stroke()
    context.restore()
  }
}

case class Point(x: Double, y: Double) {
  def +(p: Point) = Point(x + p.x, y + p.y)
  def /(d: Double) = Point(x / d, y / d)
  def up(d: Double) = Point(x, y - d)
  def down(d: Double) = Point(x, y + d)
  def left(d: Double) = Point(x - d, y)
  def right(d: Double) = Point(x + d, y)
}