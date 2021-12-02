import scala.io.Source

object FileReader {

  def fileReader(file: String): Seq[Int] = {
    val inputStream = getClass.getResourceAsStream(file)
    Source.fromInputStream(inputStream).getLines().map(_.toInt).toSeq
  }

}
