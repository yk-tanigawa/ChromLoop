/**
 * Created by yosuke on 10/31/15.
 * main function
 */
object ChromLoop extends App{
  println("welcome to Chrom Loop")

  val fasta = "ATGCGCGCGCGTTTAAAAAAT"
  val seq = new Sequence(fasta)
  seq.to_str()

  seq.kmerCount(2)


}
