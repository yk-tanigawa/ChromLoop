/**
 * Created by yosuke on 10/31/15.
 * main function
 */
object ChromLoop extends App{
  println("----- welcome to ChromLoop -----")


  val k = 3
  val chr = 21
  val res = 1000
  val binSize = res
  val norm = Option("KR")
  val expected = Option("KR")
  val min = 0
  val max = 1000000

  /* Genome */
  val fasta = new ReadFasta("./data/GRCh38.p2.ch21.fasta")
  val bin = new GenomeBins(fasta.sequence, binSize, k - 1)
  val countVector = bin.kmerCount(k)

  /* Hi-C */
  val hic = new ReadHiC("./data/GM12878_combined", s"chr$chr", res, norm, expected, min, max)
  //val m = hic.data(norm, expected, min, max)



}
