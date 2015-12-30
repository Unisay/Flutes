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

  @JSExport
  override def main() = {
    val canvas = dom.document.getElementById("fluteCanvas").asInstanceOf[html.Canvas]
    implicit val context2D = canvas.getContext("2d").asInstanceOf[C2D]
//     drawFlutes((5, 20), parseTones)
    drawFlute((20, 20), Tone.C)
  }

  private def parseTones: Seq[Seq[Tone.Value]] = {
    println(window.location.search.stripPrefix("?tones="))
    window.location.search.stripPrefix("?tones=").split("%0D%0A").toSeq.map {
      _.filter("abcdefgABCDEFG".contains(_)).map(_.toString).map(Tone.withName)
    }
  }

  private def drawFlutes(start: Vector2d, toneRows: Seq[Seq[Tone.Value]])(implicit context: C2D) =
    toneRows.foldLeft(start) { (point, tones) => drawRow(point, tones); point down 200 }

  private def drawRow(start: Vector2d, tones: Seq[Tone.Value])(implicit context: C2D) =
    tones.foldLeft(start) { (point, tone) => drawFlute(point, tone); point right 60 }

  private def drawFlute(upperLeft: Vector2d, tone: Tone.Value, width: Double = 26, height: Double = 160)(implicit context: C2D) = {
    val halfWidth = width / 2

    def drawHole(offset: Double, closed: Int)(implicit context: C2D) =
      circle(upperLeft right halfWidth down offset, halfWidth / 3, if (closed == 1) Some("#888") else None)

    def drawTone(tone: Tone.Value) = {
      val offsets = List(0, 30, 45, 60, 90, 105, 120, 150)
      for (h <- offsets.zip(states(tone))) drawHole(h._1, h._2)
    }

    def drawMund(s: Vector2d, height: Double, topWidth: Double, bottomWidth: Double)(implicit context: C2D) = {
      val deltaWidth = bottomWidth - topWidth
      val topLeft = s right deltaWidth / 2
      val topRight = topLeft right topWidth
      val bottomLeft = s down height
      val bottomRight = bottomLeft right bottomWidth

      strokePath { implicit context ⇒
        moveTo(topLeft)
        lineTo(topRight)
        bezierTo(topRight down 5, bottomRight up height / 2 right 5, bottomRight)
        moveTo(bottomLeft)
        bezierTo(bottomLeft up height / 2 left 5, topLeft down 5, topLeft)
      }
    }

    def drawRing(s: Vector2d, width: Double, height: Double, sideRadius: Double = 4, topRadius: Double = 6)(implicit context: C2D) = {
      strokePath { implicit context ⇒
        moveTo(s)
        quadraticTo(s left sideRadius down height / 2, s down height)
        quadraticTo(s right width / 2 down height + topRadius, s right width down height)
        quadraticTo(s right width + sideRadius down height / 2, s right width)
        quadraticTo(s right width / 2 down topRadius, s)
      }
    }

    def drawTop(s: Vector2d, height: Double, topWidth: Double, bottomWidth: Double)(implicit context: C2D) = {
      strokePath { implicit context ⇒
        moveTo(s)
        lineTo(s right topWidth - bottomWidth down height)
        moveTo(s right bottomWidth down height)
        lineTo(s right topWidth)
      }
    }

    def drawAirHole(topLeft: Vector2d, width: Double, height: Double, radius: Double = .8)(implicit context: C2D) = {
      val topCenter = topLeft right (width / 2)
      val bottomCenter = topCenter down height
      val topRight = topLeft right width
      val delta = 0.3
      val bottomLeft = topLeft down height right delta
      val bottomRight = topRight down height left delta

      val cutHeight = 3 * height
      val cutDelta = 2

      strokePath { implicit context ⇒
        moveTo(topLeft)
        lineTo(topLeft down cutHeight left cutDelta)
        quadraticTo(bottomCenter down cutHeight down 3 * radius, topRight down cutHeight right cutDelta)
        lineTo(topRight)
      }

      strokePath { implicit context ⇒
        moveTo(topLeft)
        quadraticTo(topCenter down radius, topRight)
        lineTo(bottomRight)
        quadraticTo(bottomCenter down radius, bottomLeft)
        lineTo(topLeft)
        fill("#222")
      }
    }

    val mundStart = upperLeft
    val mundHeight = 30
    val mundTopWidth = 15
    val mundBottomWidth = 26

    drawMund(mundStart, mundHeight, mundTopWidth, mundBottomWidth)

    val ringStart = mundStart down mundHeight
    val ringWidth = mundBottomWidth
    val ringHeight = 6

    drawRing(ringStart, ringWidth, ringHeight)

    val topStart = ringStart down ringHeight
    val topHeight = 60
    val topTopWidth = ringWidth
    val topWidthDelta = 4
    val topBottomWidth = topTopWidth - topWidthDelta

    drawTop(topStart, topHeight, topTopWidth, topBottomWidth)

    val holeWidth = topTopWidth / 3
    val holeHeight = 5

    drawAirHole(topStart right (topTopWidth - holeWidth) / 2 down 3, holeWidth, holeHeight)

    val ring2Start = topStart down topHeight right topWidthDelta
    val ring2Width = topBottomWidth - topWidthDelta
    val ring2Height = ringHeight

    drawRing(ring2Start, ring2Width, ring2Height)


//    drawTone(tone)

    //    val label = upperLeft down height down 30 right halfWidth / 2
    //    context.font = "bold 14pt Arial"
    //    context.fillText(tone.toString, label.x, label.y)
  }

  private def circle(center: Vector2d, radius: Double, fillColor: Option[String] = None)(implicit context: C2D) = {
    strokePath { implicit context ⇒
      context.arc(center.x, center.y, radius, 0, 2 * PI)
      fillColor foreach fill
    }
  }


  private def fill(style: String)(implicit context: C2D) = {
    val backup = context.fillStyle
    context.fillStyle = style
    context.fill()
    context.fillStyle = backup
  }

  private def moveTo(v: Vector2d)(implicit context: C2D, scale: Scale) =
    context.moveTo(v.x * scale.x, v.y * scale.y)

  private def lineTo(v: Vector2d)(implicit context: C2D, scale: Scale) =
    context.lineTo(v.x * scale.x, v.y * scale.y)

  private def fillRect(v: Vector2d, wh: Vector2d)(implicit context: C2D, scale: Scale) =
    context.fillRect(
      v.x * scale.x, v.y * scale.y,
      wh.x * scale.x, wh.y * scale.y)

  private def bezierTo(c1: Vector2d, c2: Vector2d, to: Vector2d)(implicit context: C2D) =
    context.bezierCurveTo(
      c1.x * scale.x, c1.y * scale.y,
      c2.x * scale.x, c2.y * scale.y,
      to.x * scale.x, to.y * scale.y)

  private def quadraticTo(c: Vector2d, to: Vector2d)(implicit context: C2D) =
    context.quadraticCurveTo(
      c.x * scale.x, c.y * scale.y,
      to.x * scale.x, to.y * scale.y)

  private def strokePath(f: C2D => Unit)(implicit context: C2D) = {
    context.beginPath()
    f(context)
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