import scala.io.Source

object FileReader {

  def intFileReader(file: String): Seq[Int] = {
    stringFileReader(file).map(_.toInt)
  }

  def stringFileReader(file: String): Seq[String] = {
    val inputStream = getClass.getResourceAsStream(file)
    Source.fromInputStream(inputStream).getLines().toSeq
  }

}
