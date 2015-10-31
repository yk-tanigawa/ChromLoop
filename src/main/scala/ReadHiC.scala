import scala.io.Source
/**
 * Created by yosuke on 11/1/15.
 * read HiC data from File
 */
class ReadHiC(datasetPath : String, chr : String, res : Int) {
  private val path = Array(datasetPath, res2resstr(res) + "_resolution_intrachromosomal", chr, "MAPQGE30").mkString("/")

  /* read various normalize vectors*/
  private val KRnorm = readNorm("KR")
  private val VCnorm = readNorm("VC")
  private val SQRTVCnorm = readNorm("SQRTVC")

  /* read various expectation values*/
  private val RAWexpected = readExpected("RAW")
  private val KRexpected = readExpected("KR")
  private val VCexpected = readExpected("VC")
  private val SQRTexpected = readExpected("SQRTVC")

  /* read raw contact frequency matrix */
  private val RAWobserved = readRawObserved

  println(path)
  println(chr)
  println(res2resstr(res))

  println(KRnorm.length)

  def this(fpath : String, chr : Int, res : Int) = {
    this(fpath, s"chr$chr", res)
  }

  private def readRawObserved = {
    val f = Source.fromFile(Array(path, "/", chr, "_", res2resstr(res), ".RAWobserved").mkString(""))
    val buf = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Double)]
    try{
      for (line <- f.getLines()) {
        val lineElements = line split "\t"
        buf.append((lineElements(0).toInt, lineElements(1).toInt, lineElements(2).toDouble))
      }
    } finally {
      f.close()
    }
    buf.toArray
  }

  private def read(method : String, dataType : String) = {
    /* read normalization vector / expected value */
    val f = Source.fromFile(Array(path, "/", chr, "_", res2resstr(res), ".", method, dataType).mkString(""))
    val buf = scala.collection.mutable.ArrayBuffer.empty[Double]
    try{
      for (line <- f.getLines()) {
        buf.append(line.toDouble)
      }
    } finally {
      f.close()
    }
    buf.toArray
  }

  private def readNorm(method : String) = read(method, "norm")

  private def readExpected(method : String) = read(method, "expected")

  private def res2resstr(res : Int) : String = {
    res match {
      case 1000    => "1kb"
      case 5000    => "5kb"
      case 10000   => "10kb"
      case 25000   => "25kb"
      case 50000   => "50kb"
      case 100000  => "100kb"
      case 250000  => "250kb"
      case 500000  => "500kb"
      case 1000000 => "1Mb"
      case _ => throw new ResSizeException(res)
    }
  }

}
