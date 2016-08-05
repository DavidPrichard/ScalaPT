package scalapt

import java.awt._
import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import java.io.File
import java.time.temporal.ChronoUnit
import java.time.{LocalDateTime => DateTime}
import javax.imageio.ImageIO


object MainFrame {
  def main(args: Array[String]): Unit = {

    val inFile     = if (args.length > 0) args(0) else "scenes/cornell.json"
    val width      = if (args.length > 1) Integer.parseInt(args(1)) else 1024
    val height     = if (args.length > 2) Integer.parseInt(args(2)) else 768
    val iterations = if (args.length > 3) Integer.parseInt(args(3)) else 1024
    val outFile    = if (args.length > 4) Option(new File(args(4))) else None

    new MainFrame("ScalaPT", width, height, iterations, inFile, outFile)
  }
}

class MainFrame(
   title: String,
   val w: Int,
   val h: Int,
   val iterations: Int,
   val inFile: String,
   val outFile: Option[File]
               ) extends Frame(title) {

  println(s"Scene: $inFile Width: $w Height: $h Iterations: $iterations")
  outFile.foreach(name => println("Outfile: " + name))


  val scene      = SceneIO.load(inFile)
  val ins        = getInsets
  val rt         = new RayTracer(w, h, scene)
  val image      = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
  val renderData = initialRenderData(w, h)


  val gr2d = image.getGraphics
      gr2d.setColor(Color.RED)
      gr2d.drawRect(0, 0, w-1, h-1)

  var closing = false

  setupWindow()

  val began = DateTime.now()

  for (i <- 0 until iterations) {
    if (!closing) {
      render(i)
      repaint()
      printStatus(i+1)
    }
  }

  saveImage(outFile)


  def render(iteration: Int) = {

    Parallel.For(0 until h) { y =>
      val row = new Array[RGB](w)
      for (x <- 0 until w) {
        val seed = (x+y*w)*(iteration+1).toLong
        row(x) = rt.sample(x, y, new XorShift(seed))
      }

      merge(renderData(y), row, iteration)

      val sy = h - y - 1

      for (sx <- 0 until w) image.setRGB(sx, sy, renderData(y)(sx).clamp.outputColour)
    }

//    Parallel.For(0 until h) { y =>
//      for (x <- 0 until w) {
//        val seed = (x+y*w)*(iteration+1)
//        renderData(y)(x) = renderData(y)(x).merge(rt.sample(x, y, new XorShift(seed)), iteration)
//        image.setRGB(x, h - y - 1, renderData(y)(x).clamp.outputColour)
//      }
//    }

    def merge(l: Array[RGB], r: Array[RGB], n: Int): Unit = {
      for (i <- l.indices)
        l(i) = l(i).merge(r(i), n)
    }
  }

  override def paint(graphics: Graphics) = {
    val g2d = graphics.asInstanceOf[Graphics2D]
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
    g2d.drawImage(image, ins.left, ins.top, null)
  }

  private def printStatus(iteration: Int): Unit = {

    val mean = ChronoUnit.MILLIS.between(began, DateTime.now()) / iteration
    println(s"Iteration: $iteration, Time per iteration: $mean ms.")
  }

  private def saveImage(outFile: Option[File]): Unit = {

    outFile match {
      case None => ()
      case Some(file) =>

        val name = file.getName

        var format = "png"

        if (name.contains('.'))
          format = name.substring(name.lastIndexOf('.') + 1)

        if (ImageIO.write(image, format, file))
          println(s"Saving to file $name")
        else
          println(s"ERROR: filename suffix $format not recognised.")
    }
  }

  private def setupWindow(): Unit = {

    // pack() // match window size to its contents

    setSize(new Dimension(w + ins.left + ins.right, h + ins.top + ins.bottom))
    setResizable(false)
    setLocationRelativeTo(null)
    setBackground(Color.BLACK)
    setVisible(true)

    addWindowListener(new WindowAdapter() {
      override def windowClosing(we: WindowEvent) = {
        closing = true
        dispose()
      }
    })

  }

  // the default "render data" for the ray-tracing to be merged into
  private def initialRenderData(width: Int, height: Int): Array[Array[RGB]] = {

    def row = new Array[RGB](width).map(x => RGB.Green)

    new Array[Array[RGB]](height).map(y => row)
  }

}
