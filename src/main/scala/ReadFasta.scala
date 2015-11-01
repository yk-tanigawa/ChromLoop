import scala.io.Source
/**
 * Created by yosuke on 10/31/15.
 * read a sequence from fasta file
 */
class ReadFasta(fname : String) {
  private val s = Source.fromFile(fname)
  private val buf = new StringBuilder

  var header = ""

  try {
    for (line <- s.getLines()) {
      if(line.startsWith(">")){
        header = line
      } else {
        buf.append(line)
      }
    }
  } finally {
    s.close()
  }

  println(s"Genome sequence file is loaded from $fname")

  def sequence = buf.result()
}
