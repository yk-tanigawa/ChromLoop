import breeze.linalg.DenseMatrix

/**
 * Created by yosuke on 10/31/15.
 * divide whole genome sequence into bins
 */
class GenomeBins(genomeSequence : String, binSize : Int, overlap : Int = 0) {
  val bins = new Array[Sequence](genomeSequence.length / binSize)

  for((bin, i) <- bins.zipWithIndex) {
    try {
      bins(i) = new Sequence(genomeSequence.slice(i * binSize, (i + 1) * binSize + overlap))
    } catch {
      case e:SeqLetterException => None
    }
  }

  println(s"Genome sequence is now devided into $length bins")

  def length = bins.length

  def kmerCount(k : Int) : Array[Option[DenseMatrix[Int]]] = {
    val c = bins.par.map {
      case s: Sequence => Some(s.kmerCount(k))
      case null => None
    }
    println(s"k-mer counting finished")
    c.toArray
  }

}
