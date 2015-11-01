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
}
