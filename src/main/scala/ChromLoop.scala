import java.io.PrintWriter
import java.util.Date
import breeze.linalg._

/**
 * Created by yosuke on 10/31/15.
 * main function
 */
object ChromLoop extends App {

  putLog("----- welcome to ChromLoop -----")

  /**
   * parameters
   */
  val k = 3
  val chr = 21
  val res = 1000
  val norm = Option("KR")
  val expected = Option("KR")
  val min = 1000
  val max = 1000000
  val dataSeq = "./data/GRCh37.ch21.fasta"
  val dataHiC = "./data/GM12878_combined"

  /**
   * some variables
   */
  val binSize = res
  val qfile = s"./tmp/res$res.k$k.chr$chr.q.out"
  val Pfile = s"./tmp/res$res.k$k.chr$chr.P.out"

  /* Genome */
  val fasta = new ReadFasta(dataSeq)
  putLog(s"Genome sequence file is loaded from " + dataSeq + " \tlength = " + fasta.length)
  val bin = new GenomeBins(fasta.sequence, binSize, k - 1)
  putLog(s"Genome sequence is now devided into " + bin.length +" bins")
  val countVector:Array[Option[DenseMatrix[Double]]] = bin.kmerCount(k)
  putLog(s"k-mer counting finished")

  /* Hi-C */
  val hic = new ReadHiC(dataHiC, s"chr$chr", res, norm, expected)
  putLog(s"Hi-C Data is loaded from " + dataHiC + " \tlength = " + hic.length)


  val params = new computeParams(hic, countVector, k, binSize, min, max)
  writeToFile(params.q, qfile)
  writeToFile(params.P, Pfile)
  putLog("computation complete:\tq : " + qfile + " \tP : " + Pfile)


  def writeToFile(m : DenseMatrix[Double], filename : String){
    val file = new PrintWriter(filename)
    val nrow:Int = m.rows
    val ncol:Int = m.cols
    file.write(s"$nrow\t$ncol\n")
    m.copy.data.foreach(f => file.write(s"$f\n"))
    file.close()
  }

  def putLog(msg : String) = {
    val date = "%tY/%<tm/%<td %<tH:%<tM:%<tS\t" format new Date
    println(date + msg)
  }

}
