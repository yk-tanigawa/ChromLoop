/**
 * Created by yosuke on 10/31/15.
 * main function
 */
object ChromLoop extends App{
  println("welcome to Chrom Loop")

  /*
  val fasta = "ATGCGCGCGCGTTTAAAAAAT"
  val seq = new Sequence(fasta)
  seq.to_str()

  seq.kmerCount(2)
*/
  val binSize = 1000
  val k = 3

  val fasta = new ReadFasta("./data/GRCh38.p2.ch21.fasta")

  val bin = new GenomeBins(fasta.sequence, binSize, k - 1)
  println(bin.length)
}
