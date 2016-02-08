import java.awt.{RenderingHints, Color, Font, Graphics2D}
import java.awt.image.BufferedImage
import GridOps._

object HanjieWord extends App {
  val text = "SIMON MILES" // NOAH
  val font = new Font ("Arial", Font.BOLD, 12)
  val image: BufferedImage = {
    val graphicsContext = new BufferedImage (1, 1, BufferedImage.TYPE_INT_RGB).getGraphics.asInstanceOf[Graphics2D]
    graphicsContext.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF)
    graphicsContext.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    graphicsContext.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
    val bounds = font.getStringBounds (text, graphicsContext.getFontRenderContext)
    val width = bounds.getWidth.asInstanceOf[Int]
    val height = bounds.getHeight.asInstanceOf[Int]
    val sizedImage = new BufferedImage (width, height, BufferedImage.TYPE_INT_RGB)
    val sizedGraphics = sizedImage.getGraphics.asInstanceOf[Graphics2D]
    sizedGraphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF)
    sizedGraphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    sizedGraphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF)
    sizedGraphics.setFont (font)
    sizedGraphics.setColor (Color.WHITE)
    sizedGraphics.fillRect (0, 0, width, height)
    sizedGraphics.setColor (Color.BLACK)
    sizedGraphics.drawString (text, 0, -bounds.getY.asInstanceOf[Int])
    sizedGraphics.dispose ()
    sizedImage
  }
  val uncompressedGrid: Seq[Seq[Boolean]] =
    for (y <- image.getMinY until image.getMinY + image.getHeight) yield
      for (x <- image.getMinX until image.getMinX + image.getWidth) yield
        image.getRGB (x, y) != -1
  val compressedGrid = uncompressedGrid.trimEdges

  val horizontal = compressedGrid  //compressedGrid.surround (true)
  val vertical = horizontal.transpose
  val width = horizontal.width
  val height = horizontal.height
  def lengths (pixels: Seq[Boolean]): Seq[Int] =
    if (pixels.isEmpty)
      Seq (0)
    else {
      val following = lengths (pixels.tail)
      if (pixels.head)
        (following.head + 1) +: following.tail
      else
        if (following.head == 0) following else 0 +: following
    }
  def allLengths (pixels: Seq[Boolean]): Seq[Int] = {
    val padded = lengths (pixels)
    if (padded.head == 0) padded.tail else padded
  }
  val rowLengths = horizontal.map (allLengths)
  val columnLenths = vertical.map (allLengths)

  for (row <- horizontal) print (allLengths (row) + " ")
  println ()
  for (column <- vertical) print (allLengths (column) + " ")
  println ()
  def rowAsString (row: Seq[Boolean]): String = row.map (b => if (b) "*" else ".").mkString
  def toString (matrix: Seq[Seq[Boolean]]): String = matrix.map (rowAsString).mkString ("\n")

  println ("Original: ")
  println (toString (horizontal))
  println ()
  println ("Solutions: ")
  val start = System.currentTimeMillis
  val solutions = Deducer.solve (rowLengths, columnLenths)
  val period = System.currentTimeMillis - start
  println ()
  println ()
  println (Show.allToGrids (solutions))
  println (solutions.size + " solution(s)")
  println ("Took: " + period + "ms")
}
