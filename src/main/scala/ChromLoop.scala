import java.io.PrintWriter
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


  val q = DenseMatrix.zeros[Double](1 << (4 * k), 1)
  hic.data.foreach {
    case Some((i, j, m)) =>
      try {
        if (countVector(i / res).isDefined && countVector(j / res).isDefined)
          q += DenseMatrix((countVector(i / res).get * countVector(j / res).get.t).copy.data.map { i: Int => i.toDouble }).t * m
        else
          None
      }catch {
        case e: ArrayIndexOutOfBoundsException =>
          println(s"i = $i\tj = $j\tres = $res\tlength(countVector) = " + countVector.length)
      }
    case None => None
  }

  println(q.t)
  writeToFile(q, "./tmp/q.out")
  println("q -- finish")


  val P = DenseMatrix.zeros[Double](1 << (4 * k), 1 << (4 * k))
  hic.data.foreach{
    case Some((i, j, m)) =>
      if(countVector(i / res).isDefined && countVector(j / res).isDefined){
        val dij = DenseMatrix((countVector(i / res).get * countVector(j / res).get.t).copy.data.map { i: Int => i.toDouble }).t * (1.0 / (binSize + k - 1))
        P += dij * dij.t
      }else
        None
    case None => None
  }

  writeToFile(P, "./tmp/P.out")
  println("P -- finish")

  def writeToFile(m : DenseMatrix[Double], filename : String){
    val file = new PrintWriter(filename)
    val nrow:Int = m.rows
    val ncol:Int = m.cols
    file.write(s"$nrow\t$ncol\n")
    m.copy.data.foreach(f => file.write(s"$f\n"))
    file.close()
  }
}
