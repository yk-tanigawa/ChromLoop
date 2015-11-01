import breeze.linalg._

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
  val min = 1000
  val max = 1000000

  /* Genome */
  val fasta = new ReadFasta("./data/GRCh38.p2.ch21.fasta")
  val bin = new GenomeBins(fasta.sequence, binSize, k - 1)
  val countVector = bin.kmerCount(k)

  /* Hi-C */
  val hic = new ReadHiC("./data/GM12878_combined", s"chr$chr", res, norm, expected, min, max)


  val qsub:scala.collection.parallel.mutable.ParArray[Option[DenseMatrix[Double]]] = hic.data.map {
    case Some((i, j, m)) =>
      if(countVector(i / res).isDefined && countVector(j / res).isDefined)
        Some(DenseMatrix((countVector(i / res).get * countVector(j / res).get.t).copy.data.map {i: Int => i.toDouble}).t * m)
      else
        None
    case None => None
  }
  println("q -- finish")

  val Psub:scala.collection.parallel.mutable.ParArray[Option[DenseMatrix[Int]]] = hic.data.map {
    case Some((i, j, m)) =>
      if(countVector(i / res).isDefined && countVector(j / res).isDefined){
        val dij = DenseMatrix((countVector(i / res).get * countVector(j / res).get.t).copy.data).t
        Some(dij * dij.t)
      }else {
        None
      }
    case None => None
  }


  println("P -- finish")
//  println(qsub)

}
